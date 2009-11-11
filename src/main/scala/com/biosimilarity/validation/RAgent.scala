// -*- mode: Scala;-*- 
// Filename:    RAgent.scala 
// Authors:     lgm                                                    
// Creation:    Tue Nov 10 16:34:49 2009 
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

trait Awareness {
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
}

case class RMessenger(
  name : URI,
  requests : ListBuffer[JustifiedRequest],
  responses : ListBuffer[JustifiedResponse],
  nameSpace : Option[LinkedHashMap[URI,Socialite]],
  traceMonitor : TraceMonitor
) extends Actor with Socialite with Awareness {
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
