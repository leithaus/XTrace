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

//import scala.continuations._ 
//import scala.continuations.ControlContext._ 
import scala.collection.mutable._
import scala.actors._
import Actor._

import com.thoughtworks.xstream._

trait Socialite[ReqBody,RspBody] {  
  def name : URI
  def requests : ListBuffer[JustifiedRequest[ReqBody,RspBody]]
  def responses : ListBuffer[JustifiedResponse[ReqBody,RspBody]]
  def nameSpace : Option[LinkedHashMap[URI,Socialite[ReqBody,RspBody]]]

  def traceMonitor : TraceMonitor[ReqBody,RspBody]

  def isJustified( request : JustifiedRequest[ReqBody,RspBody] ) : Boolean = {
    request match {
      case JustifiedRequest( _, _, _, _, _, None ) => true
      case JustifiedRequest( _, _, _, _, _, Some( response ) ) => {
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
  def isJustified( response : JustifiedResponse[ReqBody,RspBody] ) : Boolean = {
    response match {
      case JustifiedResponse( _, _, _, _, _, None ) => {	
	logError( response, UnjustifiedResponseNoRequest() )
	false
      }
      case JustifiedResponse( _, _, _, _, _, Some( request ) ) => {
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

  def validate( request : JustifiedRequest[ReqBody,RspBody] ) : Boolean = {
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

  def validate( response : JustifiedResponse[ReqBody,RspBody] ) : Boolean = {
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

  def handle( request : JustifiedRequest[ReqBody,RspBody] ) : Boolean = {
    false
  }
  def handle( response : JustifiedResponse[ReqBody,RspBody] ) : Boolean = {
    false
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

  def likes( dsg : URI, acq : Socialite[ReqBody,RspBody] ) : Boolean

  def introduce( dsg : URI, acq : Socialite[ReqBody,RspBody] ) : Boolean = {
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

  def markRequest( req : JustifiedRequest[ReqBody,RspBody] ) : Boolean = {
    requests += req
    true
  }
  def markRequest( req : InspectionRequest ) : Boolean = {
    //requests += req
    true
  }
  def markResponse( rsp : JustifiedResponse[ReqBody,RspBody] ) : Boolean = {
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
    request : JustifiedRequest[ReqBody,RspBody],
    error : NoNamespace
  ) : Boolean = {
    (traceMonitor.traceEvent(
      this,
      name.getFragment + ": " + "No namespace supplied!"
    ))
    true
  }
  
  def logError(
    request : JustifiedRequest[ReqBody,RspBody],
    error : UnknownRequester
  ) : Boolean = {
    (traceMonitor.traceEvent(
      this,
      name.getFragment + ": " + "Message from unknown sender!" 
    ))
    true
  }

  def logError(
    request : JustifiedRequest[ReqBody,RspBody],
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
    request : JustifiedRequest[ReqBody,RspBody],
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
	+ request.justification
      )
    ))
    true
  }

  def logError(
    request : JustifiedResponse[ReqBody,RspBody],
    error : NoNamespace
  ) : Boolean = {
    traceMonitor.traceEvent( 
      this,
      name.getFragment + ": " + "No namespace supplied!"
    )
    true
  }

  def logError(
    response : JustifiedResponse[ReqBody,RspBody],
    error : UnknownResponder
  ) : Boolean = {
    traceMonitor.traceEvent(
      this,
      name.getFragment + ": " + "Message from unknown sender!" 
    )
    true
  }  
  
  def logError(
    response : JustifiedResponse[ReqBody,RspBody],
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
    response : JustifiedResponse[ReqBody,RspBody],
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
	+ response.justification
      )
    )
    true
  }

  def logError(
    response : JustifiedResponse[ReqBody,RspBody],
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
	+ response.justification
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
    agent : Socialite[ReqBody,RspBody],
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
    request : JustifiedRequest[ReqBody,RspBody]
  ) : Boolean = {
    if ( useBraceNotation ) {
      traceMonitor.traceEvent(
	this,
	(	  
	  "<" + "conversation " + "id=\"" + request.flowId + "\">"
	  + "<requester " + "conversation=\"" + request.flowId + "\">"
	  + request.from.getFragment
	  + "</requester>"
	  + "<request " + "conversation=\"" + request.flowId + "\">"
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
	  + (request.justification match {
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
    response : JustifiedResponse[ReqBody,RspBody]
  ) : Boolean = {
    if ( useBraceNotation ) {
      traceMonitor.traceEvent(
	this,
	(
	  "<responder " + "conversation=\"" + response.flowId + "\">"
	  + response.from.getFragment
	  + "</responder>"
	  + "<response " + "conversation=\"" + response.flowId + "\">"
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
	  + (response.justification match {
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
	+ (request.justification match {
	  case None =>
	    "the right to inspect"
	  case x => x 
	})
      )
    )
    true
  }
}

// trait WWSocialite[ReqBody,RspBody]
// extends Socialite[ReqBody,RspBody] {  
//   override def requests : ListBuffer[URIJustifiedRequest[ReqBody]]
//   override def responses : ListBuffer[URIJustifiedResponse[RspBody]]

//   override def isJustified( request : URIJustifiedRequest[ReqBody] )
//   : Boolean = {
//     request match {
//       case URIJustifiedRequest( _, _, _, _, _, None ) => true
//       case URIJustifiedRequest( _, _, _, _, _, Some( response ) ) => {
// 	responses.contains( response ) match {
// 	  case false => {
// 	    logError( request, UnjustifiedRequest() )
// 	    false
// 	  }
// 	  case true => {
// 	    true
// 	  }
// 	}
//       }
//     }
//   }
//   def isJustified( response : JustifiedResponse[ReqBody,RspBody] ) : Boolean = {
//     response match {
//       case JustifiedResponse( _, _, _, _, _, None ) => {	
// 	logError( response, UnjustifiedResponseNoRequest() )
// 	false
//       }
//       case JustifiedResponse( _, _, _, _, _, Some( request ) ) => {
// 	requests.contains( request ) match {
// 	  case false => {
// 	    logError( response, UnjustifiedResponseNoMatch() )
// 	    false
// 	  }
// 	  case true => {
// 	    true
// 	  }
// 	}
//       }
//     }
//   }
//   def isJustified( response : InspectionRequest ) : Boolean = true

//   def validateTarget( msg : {def to : URI} ) : Boolean = {
//     msg.to == name
//   }
//   def validateAcquaintance( msg : {def from : URI} ) : Boolean = {
//     nameSpace match {
//       case None => false
//       case Some( map ) => {
// 	map.get( msg.from ) match {
// 	  case None => false
// 	  case Some( acq ) => true
// 	}
//       }
//     }
//   }

//   def validate( request : JustifiedRequest[ReqBody,RspBody] ) : Boolean = {
//     nameSpace match {
//       case None => {
// 	logError( request, NoNamespace() )
// 	false
//       }
//       case Some( map ) => {
// 	( validateAcquaintance( request ),
// 	  validateTarget( request ) ) match {
// 	   case ( false, _ ) => {
// 	     logError( request, UnknownRequester() )
// 	     false
// 	   }
// 	    case ( _, false ) => {
// 	      logError( request, IllTargetedRequest() )
// 	      false
// 	    }
// 	    case ( true, true ) => {
// 	      if ( isJustified( request ) ) {
// 		logJustification( request )  
// 		true
// 	      }
// 	      else {		  
// 		false
// 	      }
// 	    }
// 	  }
//       }
//     }
//   }

//   def validate( response : JustifiedResponse[ReqBody,RspBody] ) : Boolean = {
//     nameSpace match {
//       case None => {
// 	logError( response, NoNamespace() )
// 	false
//       }
//       case Some( map ) => {
// 	( validateAcquaintance( response ),
// 	 validateTarget( response ) ) match {
// 	   case ( false, _ ) => {
// 	     logError( response, UnknownResponder() )
// 	     false
// 	   }
// 	   case ( _, false ) => {
// 	     logError( response, IllTargetedResponse() )
// 	     false
// 	   }
// 	   case ( true, true ) => {
// 	     if ( isJustified( response ) ) {
// 	       logJustification( response )  
// 	       true
// 	     }
// 	     else {		  
// 	       false
// 	     }
// 	   }
// 	 }
//       }
//     }
//   }

//   def validate( request : InspectionRequest ) : Boolean = {
//     ( validateAcquaintance( request ), validateTarget( request ) ) match {
//       case ( false, _ ) => {	
// 	logError( request, UnknownRequester() )
// 	false
//       }
//       case ( _, false ) => {
// 	logError( request, IllTargetedRequest() )
// 	false
//       }
//       case ( true, true ) => {
// 	if ( isJustified( request ) ) {
// 	  logJustification( request )  
// 	  true
// 	}
// 	else {
// 	  throw new Exception( "should never get here" )
// 	  false
// 	}
//       }
//     }
//   }

//   def handle( request : JustifiedRequest[ReqBody,RspBody] ) : Boolean = {
//     false
//   }
//   def handle( response : JustifiedResponse[ReqBody,RspBody] ) : Boolean = {
//     false
//   }
//   def handle( request : InspectRequests ) : Boolean = {
//     println( "*** Requests ***" )
//     for (req <- requests) yield {
//       println( req )
//     }
//     println( "*** Requests ***" )
//     true
//   }
//   def handle( request : InspectResponses ) : Boolean = {
//     println( "*** Responses ***" )
//     for (rsp <- responses) yield {
//       println( rsp )
//     }
//     println( "*** Responses ***" )
//     true
//   }
//   def handle( request : InspectNamespace ) : Boolean = {
//     println( "*** Namespace ***" )
//     for (pair <- nameSpace) yield {
//       println( pair )
//     }
//     println( "*** Namespace ***" )
//     true
//   }

//   def likes( dsg : URI, acq : Socialite[ReqBody,RspBody] ) : Boolean

//   def introduce( dsg : URI, acq : Socialite[ReqBody,RspBody] ) : Boolean = {
//     if ( likes( dsg, acq ) ) {
//       nameSpace match {
// 	case None => {
// 	  logError( dsg, acq, NoNamespace() )
// 	}
// 	case Some( map ) => {
// 	  map.update( dsg, acq )
// 	}
//       }
//       true
//     }
//     else { false }
//   }

//   // Record messages for justification

//   def markRequest( req : JustifiedRequest[ReqBody,RspBody] ) : Boolean = {
//     requests += req
//     true
//   }
//   def markRequest( req : InspectionRequest ) : Boolean = {
//     //requests += req
//     true
//   }
//   def markResponse( rsp : JustifiedResponse[ReqBody,RspBody] ) : Boolean = {
//     responses += rsp
//     true
//   }

//   // Logging
  
//   trait FauxPas
//   case class NoNamespace() extends FauxPas
//   case class IllTargetedRequest() extends FauxPas
//   case class UnjustifiedRequest() extends FauxPas
//   case class UnknownRequester() extends FauxPas
//   case class IllTargetedResponse() extends FauxPas
//   case class UnjustifiedResponseNoRequest() extends FauxPas
//   case class UnjustifiedResponseNoMatch() extends FauxPas
//   case class UnknownResponder() extends FauxPas
  
//   // Error logging

//   def logError(
//     request : JustifiedRequest[ReqBody,RspBody],
//     error : NoNamespace
//   ) : Boolean = {
//     (traceMonitor.traceEvent(
//       this,
//       name.getFragment + ": " + "No namespace supplied!"
//     ))
//     true
//   }
  
//   def logError(
//     request : JustifiedRequest[ReqBody,RspBody],
//     error : UnknownRequester
//   ) : Boolean = {
//     (traceMonitor.traceEvent(
//       this,
//       name.getFragment + ": " + "Message from unknown sender!" 
//     ))
//     true
//   }

//   def logError(
//     request : JustifiedRequest[ReqBody,RspBody],
//     error : IllTargetedRequest
//   ) : Boolean = {
//     (traceMonitor.traceEvent(
//       this,
//       (
// 	name.getFragment
// 	+ ": "
// 	+ "Message intended for "
// 	+ request.to.getFragment
// 	+ " sent to wrong recipient!" 
//       )
//     ))
//     true
//   }

//   def logError(
//     request : JustifiedRequest[ReqBody,RspBody],
//     error : UnjustifiedRequest
//   ) : Boolean = {
//     (traceMonitor.traceEvent(
//       this,
//       (
// 	name.getFragment
// 	+ ": "
// 	+ request.from.getFragment
// 	+ " requested "
// 	+ request.body
// 	+ " without valid justification : "
// 	+ request.justification
//       )
//     ))
//     true
//   }

//   def logError(
//     request : JustifiedResponse[ReqBody,RspBody],
//     error : NoNamespace
//   ) : Boolean = {
//     traceMonitor.traceEvent( 
//       this,
//       name.getFragment + ": " + "No namespace supplied!"
//     )
//     true
//   }

//   def logError(
//     response : JustifiedResponse[ReqBody,RspBody],
//     error : UnknownResponder
//   ) : Boolean = {
//     traceMonitor.traceEvent(
//       this,
//       name.getFragment + ": " + "Message from unknown sender!" 
//     )
//     true
//   }  
  
//   def logError(
//     response : JustifiedResponse[ReqBody,RspBody],
//     error : IllTargetedResponse
//   ) : Boolean = {
//     traceMonitor.traceEvent(
//       this,
//       (
// 	name.getFragment
// 	+ ": "
// 	+ "Message intended for "
// 	+ response.to.getFragment
// 	+ " sent to wrong recipient!" 
//       )
//     )
//     true
//   }

//   def logError(
//     response : JustifiedResponse[ReqBody,RspBody],
//     error : UnjustifiedResponseNoRequest
//   ) : Boolean = {
//     traceMonitor.traceEvent(
//       this,
//       (
// 	name.getFragment
// 	+ ": "
// 	+ response.from.getFragment
// 	+ " requested "
// 	+ response.body
// 	+ " without valid justification : "
// 	+ response.justification
//       )
//     )
//     true
//   }

//   def logError(
//     response : JustifiedResponse[ReqBody,RspBody],
//     error : UnjustifiedResponseNoMatch
//   ) : Boolean = {
//     traceMonitor.traceEvent(
//       this,
//       (
// 	name.getFragment
// 	+ ": "
// 	+ response.from.getFragment
// 	+ " requested "
// 	+ response.body
// 	+ " without valid justification : "
// 	+ response.justification
//       )
//     )
//     true
//   }     

//   def logError(
//     request : InspectionRequest,
//     error : NoNamespace
//   ) : Boolean = {
//     traceMonitor.traceEvent(
//       this,
//       name.getFragment + ": " + "No namespace supplied!"
//     )
//     true
//   }

//   def logError(
//     dsg : URI,
//     agent : Socialite[ReqBody,RspBody],
//     error : NoNamespace
//   ) : Boolean = {
//     traceMonitor.traceEvent(
//       this,
//       dsg.getFragment + ": " + "No namespace supplied!"
//     )
//     true
//   }

//   def logError(
//     request : InspectionRequest,
//     error : UnknownRequester
//   ) : Boolean = {
//     traceMonitor.traceEvent(
//       this,
//       name.getFragment + ": " + "Message from unknown sender!" 
//     )
//     true
//   }

//   def logError(
//     request : InspectionRequest,
//     error : IllTargetedRequest
//   ) : Boolean = {
//     traceMonitor.traceEvent(
//       this,
//       (
// 	name.getFragment
// 	+ ": "
// 	+ "Message intended for "
// 	+ request.to.getFragment
// 	+ " sent to wrong recipient!" 
//       )
//     )
//     true
//   }

//   // Justification logging
  
//   def useBraceNotation : Boolean

//   def logJustification(
//     request : JustifiedRequest[ReqBody,RspBody]
//   ) : Boolean = {
//     if ( useBraceNotation ) {
//       traceMonitor.traceEvent(
// 	this,
// 	(	  
// 	  "<" + "conversation " + "id=\"" + request.flowId + "\">"
// 	  + "<requester " + "conversation=\"" + request.flowId + "\">"
// 	  + request.from.getFragment
// 	  + "</requester>"
// 	  + "<request " + "conversation=\"" + request.flowId + "\">"
// 	  + request.body
// 	  + "</request>"
// 	)
//       )
//     }
//     else {
//       traceMonitor.traceEvent(
// 	this,
// 	(
// 	  name.getFragment
// 	  + ": "
// 	  + request.from.getFragment
// 	  + " requested "
// 	  + request.body
// 	  + " justified by "
// 	  + (request.justification match {
// 	    case None =>
// 	      "the right to open a conversation"
// 	    case x => x 
// 	  })
// 	)
//       )
//     }
//     true
//   }
//   def logJustification(
//     response : JustifiedResponse[ReqBody,RspBody]
//   ) : Boolean = {
//     if ( useBraceNotation ) {
//       traceMonitor.traceEvent(
// 	this,
// 	(
// 	  "<responder " + "conversation=\"" + response.flowId + "\">"
// 	  + response.from.getFragment
// 	  + "</responder>"
// 	  + "<response " + "conversation=\"" + response.flowId + "\">"
// 	  + response.body
// 	  + "</response>"
// 	  + "<" + "/" + "conversation" + ">"
// 	)
//       )
//     }
//     else {
//       traceMonitor.traceEvent(
// 	this,
// 	(
// 	  name.getFragment
// 	  + ": "
// 	  + response.from.getFragment
// 	  + " responded "
// 	  + response.body
// 	  + " justified by "
// 	  + (response.justification match {
// 	    case None =>
// 	      throw new Exception( "should never get here" ) 
// 	    case x => x 
// 	  })
// 	)
//       )
//     }
//     true
//   }
//   def logJustification(
//     request : InspectionRequest
//   ) : Boolean = {
//     traceMonitor.traceEvent(
//       this,
//       (
// 	name.getFragment
// 	+ ": "
// 	+ request.from.getFragment
// 	+ " requested "
// 	+ request.body
// 	+ " justified by "
// 	+ (request.justification match {
// 	  case None =>
// 	    "the right to inspect"
// 	  case x => x 
// 	})
//       )
//     )
//     true
//   }
// }

case class Messenger[ReqBody,RspBody](
  name : URI,
  requests : ListBuffer[JustifiedRequest[ReqBody,RspBody]],
  responses : ListBuffer[JustifiedResponse[ReqBody,RspBody]],
  nameSpace : Option[LinkedHashMap[URI, Socialite[ReqBody,RspBody]]],
  traceMonitor : TraceMonitor[ReqBody,RspBody]
) extends Actor with Socialite[ReqBody,RspBody] {
  override def useBraceNotation : Boolean = false
  def likes( dsg : URI, acq : Socialite[ReqBody,RspBody] ) : Boolean = true
  def act () {
    nameSpace match {
      case None => {
	logError( name, this, NoNamespace() )
      }
      case Some( map ) => {
	receive {
	  case JustifiedRequest(
	    m, p, d, t,
	    f : ReqBody,
	    c : Option[Response[AbstractJustifiedRequest[ReqBody,RspBody],RspBody]]
	  ) => {
	    val jr = JustifiedRequest[ReqBody,RspBody]( m, p, d, t, f, c )
	    if ( validate( jr ) ) {
	      println( "calling handle on " + jr )
	      handle( jr )
	    }
	    act()
	  }
	  case JustifiedResponse(
	    m, p, d, t,
	    f : RspBody,
	    c : Option[Request[AbstractJustifiedResponse[ReqBody,RspBody],ReqBody]]
	  ) =>  {
	    val jr = JustifiedResponse[ReqBody,RspBody]( m, p, d, t, f, c )
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







