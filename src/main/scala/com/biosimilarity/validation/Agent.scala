// -*- mode: Scala;-*- 
// Filename:    TheMessengers.scala 
// Authors:     lgm                                                    
// Creation:    Fri Oct 16 14:23:47 2009 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.validation

import java.net.URI
import com.eaio.uuid.UUID

import scala.continuations._ 
import scala.continuations.ControlContext._ 
import scala.collection.mutable._
import scala.actors._
import Actor._

import com.thoughtworks.xstream._

trait Status[Msg] {
  def message : Msg
  def success : Boolean
  def explanation : Option[String]
  def continuation : Option[Status[Msg] => Status[Msg]]
}

abstract class HandlingStatus[Msg] (
  message : Msg,
  success : Boolean,
  explanation : Option[String],
  continuation : Option[Status[Msg] => Status[Msg]]
) extends Status[Msg]

case class JReqStatus(
  message : JustifiedRequest,
  success : Boolean,
  explanation : Option[String],
  continuation : Option[Status[JustifiedRequest] => Status[JustifiedRequest]]
) extends HandlingStatus[JustifiedRequest](
  message, success, explanation, continuation
)

case class JRspStatus(
  message : JustifiedResponse,
  success : Boolean,
  explanation : Option[String],
  continuation : Option[Status[JustifiedResponse] => Status[JustifiedResponse]]
) extends HandlingStatus[JustifiedResponse](
  message, success, explanation, continuation
)

trait Socialite {  
  def name : URI
  def requests : ListBuffer[JustifiedRequest]
  def responses : ListBuffer[JustifiedResponse]
  def nameSpace : Option[LinkedHashMap[URI,Socialite]]

  def traceMonitor : TraceMonitor

  def isJustified( request : JustifiedRequest ) : Boolean = {
    request match {
      case JustifiedRequest( _, _, None, _, _, _ ) => true
      case JustifiedRequest( _, _, Some( response ), _, _, _ ) => {
	responses.contains( response ) match {
	  case false => {
	    logError( request, UnjustifiedRequest() )
	    false
	  }
	  case true => {
	    true
	  }
	}
      }
    }
  }
  def isJustified( response : JustifiedResponse ) : Boolean = {
    response match {
      case JustifiedResponse( _, _, None, _, _, _ ) => {	
	logError( response, UnjustifiedResponseNoRequest() )
	false
      }
      case JustifiedResponse( _, _, Some( request ), _, _, _ ) => {
	requests.contains( request ) match {
	  case false => {
	    logError( response, UnjustifiedResponseNoMatch() )
	    false
	  }
	  case true => {
	    true
	  }
	}
      }
    }
  }
  def isJustified( response : InspectionRequest ) : Boolean = true

  def validateTarget( msg : {def to : URI} ) : Boolean = {
    msg.to == name
  }
  def validateAcquaintance( msg : {def from : URI} ) : Boolean = {
    nameSpace match {
      case None => false
      case Some( map ) => {
	map.get( msg.from ) match {
	  case None => false
	  case Some( acq ) => true
	}
      }
    }
  }

  def validate( request : JustifiedRequest ) : Boolean = {
    nameSpace match {
      case None => {
	logError( request, NoNamespace() )
	false
      }
      case Some( map ) => {
	( validateAcquaintance( request ),
	  validateTarget( request ) ) match {
	   case ( false, _ ) => {
	     logError( request, UnknownRequester() )
	     false
	   }
	    case ( _, false ) => {
	      logError( request, IllTargetedRequest() )
	      false
	    }
	    case ( true, true ) => {
	      if ( isJustified( request ) ) {
		logJustification( request )  
		true
	      }
	      else {		  
		false
	      }
	    }
	  }
      }
    }
  }

  def validate( response : JustifiedResponse ) : Boolean = {
    nameSpace match {
      case None => {
	logError( response, NoNamespace() )
	false
      }
      case Some( map ) => {
	( validateAcquaintance( response ),
	 validateTarget( response ) ) match {
	   case ( false, _ ) => {
	     logError( response, UnknownResponder() )
	     false
	   }
	   case ( _, false ) => {
	     logError( response, IllTargetedResponse() )
	     false
	   }
	   case ( true, true ) => {
	     if ( isJustified( response ) ) {
	       logJustification( response )  
	       true
	     }
	     else {		  
	       false
	     }
	   }
	 }
      }
    }
  }

  def validate( request : InspectionRequest ) : Boolean = {
    ( validateAcquaintance( request ), validateTarget( request ) ) match {
      case ( false, _ ) => {	
	logError( request, UnknownRequester() )
	false
      }
      case ( _, false ) => {
	logError( request, IllTargetedRequest() )
	false
      }
      case ( true, true ) => {
	if ( isJustified( request ) ) {
	  logJustification( request )  
	  true
	}
	else {
	  throw new Exception( "should never get here" )
	  false
	}
      }
    }
  }

  def handle( request : JustifiedRequest ) : Boolean = {
    false
  }
  def handle( response : JustifiedResponse ) : Boolean = {
    false
  }
  def handleWithContinuation(
    request : JustifiedRequest,
    k : Status[JustifiedRequest] => Status[JustifiedRequest]
  ) : Status[JustifiedRequest] = {
    JReqStatus( request, false, None, Some( k ) )
  }
  def handleWithContinuation(
    response : JustifiedResponse,
    k : Status[JustifiedResponse] => Status[JustifiedResponse]
  ) : Status[JustifiedResponse] = {
    JRspStatus( response, false, None, Some( k ) )
  }
  def handle( request : InspectRequests ) : Boolean = {
    println( "*** Requests ***" )
    for (req <- requests) yield {
      println( req )
    }
    println( "*** Requests ***" )
    true
  }
  def handle( request : InspectResponses ) : Boolean = {
    println( "*** Responses ***" )
    for (rsp <- responses) yield {
      println( rsp )
    }
    println( "*** Responses ***" )
    true
  }
  def handle( request : InspectNamespace ) : Boolean = {
    println( "*** Namespace ***" )
    for (pair <- nameSpace) yield {
      println( pair )
    }
    println( "*** Namespace ***" )
    true
  }

  def likes( dsg : URI, acq : Socialite ) : Boolean

  def introduce( dsg : URI, acq : Socialite ) : Boolean = {
    if ( likes( dsg, acq ) ) {
      nameSpace match {
	case None => {
	  logError( dsg, acq, NoNamespace() )
	}
	case Some( map ) => {
	  map.update( dsg, acq )
	}
      }
      true
    }
    else { false }
  }

  // Record messages for justification

  def markRequest( req : JustifiedRequest ) : Boolean = {
    requests += req
    true
  }
  def markRequest( req : InspectionRequest ) : Boolean = {
    //requests += req
    true
  }
  def markResponse( rsp : JustifiedResponse ) : Boolean = {
    responses += rsp
    true
  }

  // Logging
  
  trait FauxPas
  case class NoNamespace() extends FauxPas
  case class IllTargetedRequest() extends FauxPas
  case class UnjustifiedRequest() extends FauxPas
  case class UnknownRequester() extends FauxPas
  case class IllTargetedResponse() extends FauxPas
  case class UnjustifiedResponseNoRequest() extends FauxPas
  case class UnjustifiedResponseNoMatch() extends FauxPas
  case class UnknownResponder() extends FauxPas
  
  // Error logging

  def logError(
    request : JustifiedRequest,
    error : NoNamespace
  ) : Boolean = {
    (traceMonitor.traceEvent(
      this,
      name.getFragment + ": " + "No namespace supplied!"
    ))
    true
  }
  
  def logError(
    request : JustifiedRequest,
    error : UnknownRequester
  ) : Boolean = {
    (traceMonitor.traceEvent(
      this,
      name.getFragment + ": " + "Message from unknown sender!" 
    ))
    true
  }

  def logError(
    request : JustifiedRequest,
    error : IllTargetedRequest
  ) : Boolean = {
    (traceMonitor.traceEvent(
      this,
      (
	name.getFragment
	+ ": "
	+ "Message intended for "
	+ request.to.getFragment
	+ " sent to wrong recipient!" 
      )
    ))
    true
  }

  def logError(
    request : JustifiedRequest,
    error : UnjustifiedRequest
  ) : Boolean = {
    (traceMonitor.traceEvent(
      this,
      (
	name.getFragment
	+ ": "
	+ request.from.getFragment
	+ " requested "
	+ request.body
	+ " without valid justification : "
	+ request.deeper
      )
    ))
    true
  }

  def logError(
    request : JustifiedResponse,
    error : NoNamespace
  ) : Boolean = {
    traceMonitor.traceEvent( 
      this,
      name.getFragment + ": " + "No namespace supplied!"
    )
    true
  }

  def logError(
    response : JustifiedResponse,
    error : UnknownResponder
  ) : Boolean = {
    traceMonitor.traceEvent(
      this,
      name.getFragment + ": " + "Message from unknown sender!" 
    )
    true
  }  
  
  def logError(
    response : JustifiedResponse,
    error : IllTargetedResponse
  ) : Boolean = {
    traceMonitor.traceEvent(
      this,
      (
	name.getFragment
	+ ": "
	+ "Message intended for "
	+ response.to.getFragment
	+ " sent to wrong recipient!" 
      )
    )
    true
  }

  def logError(
    response : JustifiedResponse,
    error : UnjustifiedResponseNoRequest
  ) : Boolean = {
    traceMonitor.traceEvent(
      this,
      (
	name.getFragment
	+ ": "
	+ response.from.getFragment
	+ " requested "
	+ response.body
	+ " without valid justification : "
	+ response.deeper
      )
    )
    true
  }

  def logError(
    response : JustifiedResponse,
    error : UnjustifiedResponseNoMatch
  ) : Boolean = {
    traceMonitor.traceEvent(
      this,
      (
	name.getFragment
	+ ": "
	+ response.from.getFragment
	+ " requested "
	+ response.body
	+ " without valid justification : "
	+ response.deeper
      )
    )
    true
  }     

  def logError(
    request : InspectionRequest,
    error : NoNamespace
  ) : Boolean = {
    traceMonitor.traceEvent(
      this,
      name.getFragment + ": " + "No namespace supplied!"
    )
    true
  }

  def logError(
    dsg : URI,
    agent : Socialite,
    error : NoNamespace
  ) : Boolean = {
    traceMonitor.traceEvent(
      this,
      dsg.getFragment + ": " + "No namespace supplied!"
    )
    true
  }

  def logError(
    request : InspectionRequest,
    error : UnknownRequester
  ) : Boolean = {
    traceMonitor.traceEvent(
      this,
      name.getFragment + ": " + "Message from unknown sender!" 
    )
    true
  }

  def logError(
    request : InspectionRequest,
    error : IllTargetedRequest
  ) : Boolean = {
    traceMonitor.traceEvent(
      this,
      (
	name.getFragment
	+ ": "
	+ "Message intended for "
	+ request.to.getFragment
	+ " sent to wrong recipient!" 
      )
    )
    true
  }

  // Justification logging
  
  def useBraceNotation : Boolean

  def logJustification(
    request : JustifiedRequest
  ) : Boolean = {
    if ( useBraceNotation ) {
      traceMonitor.traceEvent(
	this,
	(	  
	  "<" + "conversation " + "id=\"" + request.color + "\">"
	  + "<requester " + "conversation=\"" + request.color + "\">"
	  + request.from.getFragment
	  + "</requester>"
	  + "<request " + "conversation=\"" + request.color + "\">"
	  + request.body
	  + "</request>"
	)
      )
    }
    else {
      traceMonitor.traceEvent(
	this,
	(
	  name.getFragment
	  + ": "
	  + request.from.getFragment
	  + " requested "
	  + request.body
	  + " justified by "
	  + (request.deeper match {
	    case None =>
	      "the right to open a conversation"
	    case x => x 
	  })
	)
      )
    }
    true
  }
  def logJustification(
    response : JustifiedResponse
  ) : Boolean = {
    if ( useBraceNotation ) {
      traceMonitor.traceEvent(
	this,
	(
	  "<responder " + "conversation=\"" + response.color + "\">"
	  + response.from.getFragment
	  + "</responder>"
	  + "<response " + "conversation=\"" + response.color + "\">"
	  + response.body
	  + "</response>"
	  + "<" + "/" + "conversation" + ">"
	)
      )
    }
    else {
      traceMonitor.traceEvent(
	this,
	(
	  name.getFragment
	  + ": "
	  + response.from.getFragment
	  + " responded "
	  + response.body
	  + " justified by "
	  + (response.deeper match {
	    case None =>
	      throw new Exception( "should never get here" ) 
	    case x => x 
	  })
	)
      )
    }
    true
  }
  def logJustification(
    request : InspectionRequest
  ) : Boolean = {
    traceMonitor.traceEvent(
      this,
      (
	name.getFragment
	+ ": "
	+ request.from.getFragment
	+ " requested "
	+ request.body
	+ " justified by "
	+ (request.deeper match {
	  case None =>
	    "the right to inspect"
	  case x => x 
	})
      )
    )
    true
  }
}

case class Messenger(
  name : URI,
  requests : ListBuffer[JustifiedRequest],
  responses : ListBuffer[JustifiedResponse],
  nameSpace : Option[LinkedHashMap[URI,Socialite]],
  traceMonitor : TraceMonitor
) extends Actor with Socialite {
  override def useBraceNotation : Boolean = false
  def likes( dsg : URI, acq : Socialite ) : Boolean = true
  def act () {
    nameSpace match {
      case None => {
	logError( name, this, NoNamespace() )
      }
      case Some( map ) => {
	receive {
	  case JustifiedRequest( m, p, d, t, f, c ) => {
	    val jr = JustifiedRequest( m, p, d, t, f, c )
	    if ( validate( jr ) ) {
	      println( "calling handle on " + jr )
	      handle( jr )
	    }
	    act()
	  }
	  case JustifiedResponse( m, p, d, t, f, c ) =>  {
	    val jr = JustifiedResponse( m, p, d, t, f, c )
	    if ( validate( jr ) ) {
	      println( "calling handle on " + jr )
	      handle( jr )
	    }
	    act()
	  }
	  case InspectRequests( t, f ) => {
	    val ir = InspectRequests( t, f )
	    if ( validate( ir ) ) {
	      println( "calling handle on " + ir )
	      handle( ir )
	    }
	    act()
	  }
	  case InspectResponses( t, f ) => {
	    val ir = InspectResponses( t, f )
	    if ( validate( ir ) ) {
	      println( "calling handle on " + ir )
	      handle( ir )
	    }
	    act()
	  }
	  case InspectNamespace( t, f ) => {
	    val ir = InspectNamespace( t, f )
	    if ( validate( ir ) ) {
	      println( "calling handle on " + ir )
	      handle( ir )
	    }
	    act()
	  }
	}
      }
    }    
  }
}

case class RMessenger(
  name : URI,
  requests : ListBuffer[JustifiedRequest],
  responses : ListBuffer[JustifiedResponse],
  nameSpace : Option[LinkedHashMap[URI,Socialite]],
  traceMonitor : TraceMonitor
) extends Actor with Socialite {
  override def useBraceNotation : Boolean = false
  def likes( dsg : URI, acq : Socialite ) : Boolean = true
  def act () {
    nameSpace match {
      case None => {
	logError( name, this, NoNamespace() )
      }
      case Some( map ) => {
	receive {
	  case JustifiedRequest( m, p, d, t, f, c ) => {
	    val jr = JustifiedRequest( m, p, d, t, f, c )
	    if ( validate( jr ) ) {
	      println( "calling handle on " + jr )
	      reset {
		shift {
		  ( k : Status[JustifiedRequest] => Status[JustifiedRequest] )
		  => {
		    k( handleWithContinuation( jr, k ) )
		  }
		}
	      }
	    }
	    act()
	  }
	  case JustifiedResponse( m, p, d, t, f, c ) =>  {
	    val jr = JustifiedResponse( m, p, d, t, f, c )
	    if ( validate( jr ) ) {
	      println( "calling handle on " + jr )
	      reset {
		shift {
		  ( k : Status[JustifiedResponse] => Status[JustifiedResponse] )
		  => {
		    k( handleWithContinuation( jr, k ) )
		  }
		}
	      }
	    }
	    act()
	  }
	  case InspectRequests( t, f ) => {
	    val ir = InspectRequests( t, f )
	    if ( validate( ir ) ) {
	      println( "calling handle on " + ir )
	      handle( ir )
	    }
	    act()
	  }
	  case InspectResponses( t, f ) => {
	    val ir = InspectResponses( t, f )
	    if ( validate( ir ) ) {
	      println( "calling handle on " + ir )
	      handle( ir )
	    }
	    act()
	  }
	  case InspectNamespace( t, f ) => {
	    val ir = InspectNamespace( t, f )
	    if ( validate( ir ) ) {
	      println( "calling handle on " + ir )
	      handle( ir )
	    }
	    act()
	  }
	}
      }
    }    
  }
}


