// -*- mode: Scala;-*- 
// Filename:    JSONAMQPDispatcher.scala 
// Authors:     lgm                                                    
// Creation:    Tue May 26 06:40:08 2009 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package net.liftweb.amqp

import _root_.com.eaio.uuid.UUID
import _root_.com.rabbitmq.client._
import _root_.scala.actors.Actor
import _root_.scala.collection.mutable.Stack
import _root_.java.io.ObjectInputStream
import _root_.java.io.ByteArrayInputStream
import _root_.java.util.Timer
import _root_.java.util.TimerTask

import _root_.com.thoughtworks.xstream.XStream
import _root_.com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

import javax.persistence.EntityManager
import javax.persistence.EntityManagerFactory
import javax.persistence.Persistence

//import org.apache.log4j.Logger
//import org.apache.log4j.PropertyConfigurator
import java.util.Properties
import java.io.FileInputStream
import java.io.IOException

class JSONSerializedAMQPDispatcher[T](
  factory: ConnectionFactory,
  host: String,
  port: Int
) extends AMQPDispatcher[T](factory, host, port) {  
  override def configure(channel: Channel) {
    // Get the ticket.
    val ticket = channel.accessRequest("/data")
    // Set up the exchange and queue
    channel.exchangeDeclare(ticket, "mult", "direct")
    channel.queueDeclare(ticket, "mult_queue")
    channel.queueBind(ticket, "mult_queue", "mult", "routeroute")
    // Use the short version of the basicConsume method for convenience.
    channel.basicConsume(ticket, "mult_queue", false, new SerializedConsumer(channel, this))
  }  
}

trait JSONHandler {
  def handle( contents: String ) : Unit = {
    new XStream( new JettisonMappedXmlDriver() ).fromXML( contents )
  }
}

trait JSONToScalaHandler {
  def handle( dummy : String )( contents: String ) : Unit = {
    new XStream( new JettisonMappedXmlDriver() ).fromXML( contents )
  }
}

trait JSONToSQLHandler {
  self : IdSupplier =>
    var _objectCaching : Boolean = true
  def objectCachingOn : Boolean = _objectCaching
  def turnObjectCachingOn : Unit = {
    _objectCaching = true
  }
  def turnObjectCachingOff : Unit = {
    _objectCaching = false
  }
  var _incoming : Option[Stack[java.lang.Object]] = None      
  def inComing : Stack[java.lang.Object] = {
    _incoming match {
      case Some( inC ) => inC
      case None => {
	val inC = new Stack[java.lang.Object] ()
	_incoming = Some( inC )
	inC
      }
    }
  }
  def recent : Option[java.lang.Object] = {
    if ( inComing.isEmpty ) {
      None      
    }
    else {
      Some( inComing.pop )
    }
  }
  def acquire( obj : java.lang.Object ) = {
    inComing.push( obj )
  }
  var _emf : Option[EntityManagerFactory] = None
  def entityMgrFactory( db : String ) : EntityManagerFactory = {
    _emf match {
      case Some( emf ) => emf
      case None => {
	val emf = Persistence.createEntityManagerFactory( db )
	_emf = Some( emf )
	emf
      }
    }
  }
  var _em : Option[EntityManager] = None
  def entityManager( db : String ) : EntityManager = {
    _em match {
      case Some( em ) => em
      case None => {
	val em = entityMgrFactory( db ).createEntityManager()
	_em = Some( em )
	em
      }
    }
  }
  def handle( db : String )( contents: String ) : Unit = {
    var obj : java.lang.Object = null;
    try {
      obj = 
	new XStream(
	  new JettisonMappedXmlDriver()
	).fromXML(
	  contents.replace(
	    "Absyn", "Absyn.persistence.sql"
	  )
	);

      if ( objectCachingOn ) {
	acquire( obj );
      }

      //generateIds( obj );
      
      try {
	entityManager( db ).getTransaction().begin();
	entityManager( db ).persist( obj );
	entityManager( db ).getTransaction().commit();
      }
      catch {
	case e => {
	  println( "persistence error attempting to store " + obj )
	  
	  e.printStackTrace
	}
      }
    }
    catch {
      case e => {
	  println( "marshaling error" )
	  e.printStackTrace
	}
    }
  }
}

class JSONAMQPListener( host : String ) {
  val LOG_PROPERTIES_FILE : String =
    "src/main/resources/Log4J.properties";  

  val params = new ConnectionParameters
  params.setUsername("guest")
  params.setPassword("guest")
  params.setVirtualHost("/")
  params.setRequestedHeartbeat(0)

  val factory = new ConnectionFactory(params)
  
  val amqp =
    new JSONSerializedAMQPDispatcher[String](
      factory,
      //"localhost",
      host,
      5672
    )

  def configureLogging() {
    // val logProperties : Properties = new Properties();
//     val log : Logger =
//       Logger.getLogger(classOf[JSONAMQPListener]);
//     try {      
//       logProperties.load(new FileInputStream(LOG_PROPERTIES_FILE));
//       PropertyConfigurator.configure(logProperties);
//       log.info("Logging initialized.");
//     }
//     catch {
//       case e => e.printStackTrace
//     }
  }
  
  def testHandle = {
    val jal = new net.liftweb.amqp.JSONAMQPListener( "localhost" )
    jal.amqp ! net.liftweb.amqp.AMQPReconnect( 4 )
    jal.jsonListener.handle( "rlambda_production" )(
      new com.thoughtworks.xstream.XStream(
	new com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver
      ).toXML(
	//(new com.biosimilarity.reflection.model.REPL).read( "lambda x.x" )
	"fix me"
      )
    )
  }

  amqp.start

  // JSON Listener
  class JSONListener(
    logging : Boolean,
    rcrs    : Boolean,
    fOUT    : Boolean
  )
  extends Actor
  //with JSONToSQLHandler
  with JSONToScalaHandler
  with IdSupplier {    
    // ID Generation
    override def recurse() = rcrs
    override def hasDepth( pojo : java.lang.Object ) : Boolean = {
      altHasDepth( pojo )
    }
    override def failOnUnknownType() = fOUT
    
    override def stdAction() : Action = {
      ( subject : ActedOn ) => {
	if (reallyHasSetId( subject ))
	  reallyCallSetId(
	    subject,
	    getNextId()
	  );
	for (field <- subject.getClass.getDeclaredFields
	     if (field.getName.contains( "idSuper" )
	       || field.getName.contains( "uuid" )))
	yield {
	  // reflectively break java access control mechanisms
	  val accessible = field.isAccessible;
	  field.setAccessible( true );

	  field.set( subject, getNextId() )
    
	  // put java access mechanisms back in place
	  field.setAccessible( accessible );    
	}
      }
    }

    override def getNextId() = {
      new UUID() + ""
    }

    def act = {
      react {
	case msg@AMQPMessage( contents : String ) => {
	  if ( logging ) {
	    println("received: " + msg)
	  };
	  handle( "stockholm" )( contents );
	  act
	}
      }
    }
  }

  val jsonListener =
    new JSONListener( true, true, false )

  jsonListener.start
  amqp ! AMQPAddListener( jsonListener )
}
