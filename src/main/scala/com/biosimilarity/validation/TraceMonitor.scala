// -*- mode: Scala;-*- 
// Filename:    TheScribe.scala 
// Authors:     lgm                                                    
// Creation:    Mon Oct 19 01:28:27 2009 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.validation

import scala.collection.mutable._
import scala.xml._
import scala.actors._
import Actor._

trait LogAction[ReqBody,RspBody]
trait Report[ReqBody,RspBody] {
  def agent : Socialite[ReqBody,RspBody]
  def message : String
}
case class TraceEvent[ReqBody,RspBody]( agent : Socialite[ReqBody,RspBody], msg : String )
     extends LogAction[ReqBody,RspBody] with Report[ReqBody,RspBody] {
       override def message = msg
     }
case class TraceXmlEvent[ReqBody,RspBody]( agent : Socialite[ReqBody,RspBody], msg : Elem ) 
     extends LogAction[ReqBody,RspBody] with Report[ReqBody,RspBody] {
       override def message = msg.toString
     }
case class ShowLog[ReqBody,RspBody]() extends LogAction[ReqBody,RspBody]

trait SessionStatus[ReqBody,RspBody]
case class OpenSession[ReqBody,RspBody]( agent : Socialite[ReqBody,RspBody] )
     extends LogAction[ReqBody,RspBody] with SessionStatus[ReqBody,RspBody]
case class CloseSession[ReqBody,RspBody]( agent : Socialite[ReqBody,RspBody] )
     extends LogAction[ReqBody,RspBody] with SessionStatus[ReqBody,RspBody]

trait DebuggingLevel 
case class Naked() extends DebuggingLevel
case class FullyClothed() extends DebuggingLevel

class TraceMonitor[ReqBody,RspBody] extends Actor {
  var debuggingLevel : DebuggingLevel = Naked()
  //var debuggingLevel : DebuggingLevel = FullyClothed()
  val sessions : LinkedHashMap[Socialite[ReqBody,RspBody],SessionStatus[ReqBody,RspBody]] =
    new LinkedHashMap[Socialite[ReqBody,RspBody],SessionStatus[ReqBody,RspBody]]()
  val messageLog : ListBuffer[Report[ReqBody,RspBody]] =
    new ListBuffer[Report[ReqBody,RspBody]]()

  def dumpLogToString : String =
    dumpLogToWriter( new java.io.StringWriter() )
  def dumpLogToXML : Sequence[Node] = {
    messageLog.map(
      {
	case TraceEvent( a, s ) => {
	  <entry>{s}</entry>
	}
      }
    )
  }
  def dumpLogToWriter( sw : java.io.StringWriter ) : String = {
    messageLog.map( { case TraceEvent( a, s ) => sw.write( s ) } )
    sw.toString()
  }
  def showLog() : Unit = {
    println(
      preamble + dumpLogToString + postScript
    )
  }
  def getFinalLog() : String = {
    while ( inSession ) {
      Thread.sleep( 500 )
    }
    preamble + dumpLogToString + postScript
  }

  def sessionStatus( agent : Socialite[ReqBody,RspBody] )
  : Option[SessionStatus[ReqBody,RspBody]] = {
    sessions.get( agent )
  }
  def openMonitoringSession( agent : Socialite[ReqBody,RspBody] )
  : OpenSession[ReqBody,RspBody] = {
    sessions.get( agent ) match {
      case Some( agent ) => {
	throw new Exception( "already in session" )
      }
      case None => {
	val session = OpenSession( agent )
	sessions.update( agent, session )
	session
      }
    }
  }
  def closeMonitoringSession( agent : Socialite[ReqBody,RspBody] )
  : CloseSession[ReqBody,RspBody] = {
    sessions.get( agent ) match {
      case Some( session ) => {
	val session = CloseSession( agent )
	sessions.update( agent, session )
	session
      }
      case None => {
	throw new Exception( "Not in session" )
      }
    }
  }

  def inSession : Boolean = {
    ( false /: sessions.values )(
      { ( acc, session ) => {
	session match {
	  case CloseSession( _ ) => acc
	  case OpenSession( _ ) => true
	  }
	}
      }
    )
  }
  
  def traceEvent( agent : Socialite[ReqBody,RspBody], msg : String ) : Unit = {
    traceEvent( TraceEvent( agent, msg ) )
  }

  def traceEvent( report : Report[ReqBody,RspBody] ) : Unit = {
    debuggingLevel match {
      case Naked() => {
	println( "logging event to console: " + report.message )
      }
    }
    sessions.get( report.agent ) match {
      case Some( OpenSession( a ) ) => {
	messageLog += report
      }
      case Some( CloseSession( a ) ) => {
	throw new Exception( "Monitoring against a closed session!" )
      }
      case None => {
	throw new Exception( "Monitoring without opening a session" )
      }
    }
  }
  
  def preamble : String = {
    //"<execution xmlns=\"http://conversation.biosimilarity.com/validate/0.0.1\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://conversation.biosimilarity.com/validate/0.0.1 http://conversation.biosimilarity.com/validate-v0_0_0.xsd\">"
    "<execution>"
  }
  def postScript : String = {
    "</execution>"
  }
  def xmlContainer : Elem = {
    <execution xmlns="http://conversation.biosimilarity.com/validate/0.0.1" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://conversation.biosimilarity.com/validate/0.0.1 http://conversation.biosimilarity.com/validate-v0_0_0.xsd">
     {dumpLogToXML}:_*
    </execution>
  }
  
  // Be very careful with this interface. Essentially all interaction
  // with a monitor must be a transaction/function-call. Otherwise,
  // you will encounter surprising race conditions.
  def act () {
    receive {
      case OpenSession( agent ) => {
	openMonitoringSession( agent.asInstanceOf[Socialite[ReqBody,RspBody]] )
	act()
      }
      case CloseSession( agent ) => {
	closeMonitoringSession( agent.asInstanceOf[Socialite[ReqBody,RspBody]] )
	act()
      }
      case TraceEvent( agent, msg ) => {
	traceEvent( TraceEvent( agent.asInstanceOf[Socialite[ReqBody,RspBody]], msg ) )
	act()
      }
      case TraceXmlEvent( agent, msg ) => {
	traceEvent( TraceXmlEvent( agent.asInstanceOf[Socialite[ReqBody,RspBody]], msg ) )
	act()
      }
      case ShowLog() => {
	showLog()
	act()
      }
    }
  }
}

object AStringTraceMonitor extends TraceMonitor[String,String]
