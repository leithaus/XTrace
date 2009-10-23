// -*- mode: Scala;-*- 
// Filename:    Bard.scala 
// Authors:     lgm                                                    
// Creation:    Wed May 27 17:12:47 2009 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package net.liftweb.amqp;    

import com.eaio.uuid.UUID;

trait IdSupplier {
  type ActedOn = {def setId( s : String ) : Unit}
  type Classic = {def getClass() : java.lang.Class[_]}
  type ClassicallyActedOn = Classic with ActedOn
  type Action = ActedOn => Unit  
  type InAction = Classic => Unit

  def recurse() : Boolean
  def hasDepth( pojo : java.lang.Object ) : Boolean = {
    pojo.isInstanceOf[ClassicallyActedOn]
  }
  def recurse( pojo : java.lang.Object ) : Boolean = {
    ((hasDepth( pojo )) && (recurse()))
  }
  def failOnUnknownType() : Boolean
  def inView(
    field : java.lang.reflect.Field,
    pojo  : Classic
  ) : Boolean = {
    true    
  }
  def isGroundValueType(
    value : {def getClass() : java.lang.Class[_]}
  ) : Boolean = {
    ((value.isInstanceOf[Boolean]) 
     || (value.isInstanceOf[Integer]) 
     || (value.isInstanceOf[Float])
     || (value.isInstanceOf[String])
     // put more ground types here
   )
  }
  def getNextId() : String
  def stdAction() : Action = {
    ( subject : ActedOn ) => {
      subject.setId( getNextId() )
    }
  }
  def inAction() = { (_ : Classic) => { } }

  def handleUnmatchedValue(
    vUnmatched : Classic,
    field      : java.lang.reflect.Field,
    pojo       : ClassicallyActedOn,
    inAction   : InAction
    ) : Unit = {
      if (! failOnUnknownType() ){
	inAction( vUnmatched )
      }
      else throw new Exception(
	(
	  "unmatched type"
	  + vUnmatched.getClass.toString
	  + "when attempting render the "
	  + field.getName
	  + "field of "
	  + pojo
	)
      )
    }
  def handleGroundValue(
    value : Classic
    ) : Unit = {
      (inAction())( value )
    }
  def handleValue( 
    value  : Classic,
    field  : java.lang.reflect.Field,
    pojo   : ClassicallyActedOn,
    action : Action
  ) : Unit = {
    if (value == null) {
      (inAction())( null )
    }
    else {
      if ( isGroundValueType( value ) ) 
	handleGroundValue( value, action )
      else if ( recurse( value ) )
	handlePOJO(
	  value.asInstanceOf[ClassicallyActedOn],
	  action
	) 
      else handleUnmatchedValue(
	value,
	field,
	pojo,
	inAction()
      )
    }
  }
  def handleField(
    field  : java.lang.reflect.Field,
    pojo   : ClassicallyActedOn,
    action : Action
  ) : Unit = {    
    // reflectively break java access control mechanisms
    val accessible = field.isAccessible;
    field.setAccessible( true );

    handleValue( field.get( pojo ), field, pojo, action )
    
    // put java access mechanisms back in place
    field.setAccessible( accessible );    
    
  }
  
  // This is the basic monadic/monad transformer view of the pojo
  // rendering process. It shouldn't be surprising that this would
  // have this form: if you stop to think about it a pojo is a relation.
  def handlePOJO(
    pojo   : ClassicallyActedOn,
    action : Action
  ) : Unit = {
    action( pojo );
    val progeny = 
      for (field <- pojo.getClass.getDeclaredFields
	   if inView( field, pojo ))
      yield handleField( field, pojo, action );
  }
  def supplyIds( pojo : ClassicallyActedOn ) : Unit = {
    handlePOJO( pojo, stdAction() )
  }
  def generateIds( obj : java.lang.Object ) : Unit = {
    supplyIds( obj.asInstanceOf[ClassicallyActedOn] )
  }

  // ugly stuff
  def reallyHasSetId( pojo : Classic ) : Boolean = {
    (for ( m <- pojo.getClass.getMethods if m.getName.contains( "setId" ) )
     yield { true }).length > 0
  }
  def reallyCallSetId(
    pojo : Classic,
    id   : String
  ) : Unit = {
// fix this
//     val setIdMethod : java.lang.reflect.Method
//     = (for ( m <- pojo.getClass.getMethods if m.getName.contains( "setId" ) )
//        yield { m })(0);
//     setIdMethod.invoke( pojo, Array( id ): _* );
  }
  def altHasDepth( pojo : java.lang.Object ) : Boolean = {
    reallyHasSetId( pojo )
  }
  def altRecurse( pojo : java.lang.Object ) : Boolean = {
    ((altHasDepth( pojo )) && (recurse()))
  }
  def altAction() : Action = {
    ( subject : ActedOn ) => {
      if (reallyHasSetId( subject ))
	reallyCallSetId(
	  subject,
	  getNextId()
	)
    }
  }
}

case class Fingerer(
  rcrs    : Boolean,
  fOUT    : Boolean
) extends IdSupplier {
  override def recurse() = rcrs
  override def failOnUnknownType() = fOUT
  override def inAction() = {
    case x : Classic => {
    }
  }
  override def getNextId() = {
    new UUID() + ""
  }
}
