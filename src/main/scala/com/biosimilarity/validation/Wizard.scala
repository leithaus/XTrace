// -*- mode: Scala;-*- 
// Filename:    Wizard.scala 
// Authors:     lgm                                                    
// Creation:    Mon Oct 19 13:15:47 2009 
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

object Cabal {  
  // Identities
  val harrysName : URI =
    new URI( "wizard", "Hogwarts", "Harry" )
  val ronsName : URI =
    new URI( "wizard", "Hogwarts", "Ron" )
  val hermionesName : URI =
    new URI( "wizard", "Hogwarts", "Herminone" )
  val dracosName : URI =
    new URI( "wizard", "Hogwarts", "Draco" )

  // Conversations
  val attraction : UUID = new UUID()
  val friendship : UUID = new UUID()
  val magicalAttack : UUID = new UUID()
  val magicalAttackTwo : UUID = new UUID()
  val magicalDefense : UUID = new UUID()
  val magicalDefenseTwo : UUID = new UUID()

  // Messages

  type AJustStringRequest = JustifiedRequest[String,String]
  type AJustStringResponse = JustifiedResponse[String,String]  

  val pickUpLine : AJustStringRequest =
    JustifiedRequest(
      new UUID(),
      hermionesName,
      ronsName,
      attraction,
      "chocolateFrog?",
      None
    )
  val friendlyOverture : AJustStringRequest =
    JustifiedRequest(
      new UUID,
      hermionesName,
      harrysName,
      friendship,
      "bean?",
      None      
    )
  val expelliarmus : AJustStringRequest =
    JustifiedRequest(
      new UUID(),
      dracosName,
      harrysName,
      magicalDefense,
      "expelliarmus",
      None      
    )
//   val expectoPatronum : AJustStringRequest =
//     JustifiedRequest( 0, "expectoPatronum", None, dracosName, harrysName )
  val confundo : AJustStringRequest =
    JustifiedRequest(
      new UUID(),
      harrysName,
      dracosName,
      magicalAttack,
      "confundo",
      None      
    )
  val impedimenta : AJustStringRequest =
    JustifiedRequest(
      new UUID(),
      ronsName,
      harrysName,
      magicalAttackTwo,
      "impedimenta",
      None      
    )
  val finiteIncantatem : AJustStringResponse =
    JustifiedResponse(
      new UUID(),
      harrysName,
      dracosName,
      magicalDefenseTwo,
      "finiteIncantatem",
      Some( expelliarmus.asInstanceOf[Request[AbstractJustifiedResponse[String,String],String]] )      
    )

  val flirt : AJustStringResponse =
    JustifiedResponse(
      new UUID(),
      ronsName,
      hermionesName,
      attraction,
      "chocolateFrog!",
      Some( pickUpLine.asInstanceOf[Request[AbstractJustifiedResponse[String,String],String]] )      
    )
  val friendlyResponse : AJustStringResponse =
    JustifiedResponse(
      new UUID(),
      harrysName,
      hermionesName,
      friendship,
      "bean.",
      Some( friendlyOverture.asInstanceOf[Request[AbstractJustifiedResponse[String,String],String]] )      
    )  

  case object MessengerOne
     extends Messenger[String,String](
       harrysName,
       new ListBuffer[AJustStringRequest](),
       new ListBuffer[AJustStringResponse](),
       Some( new LinkedHashMap[URI,Socialite[String,String]]() ),
       AStringTraceMonitor
       ) {
	 val responsesReceived : ListBuffer[AJustStringResponse] =
	   new ListBuffer[AJustStringResponse]()
	 override def useBraceNotation : Boolean = true
	 def script() = {	   
	   markRequest( friendlyOverture )
	   MessengerThree ! friendlyOverture
	   markRequest( expelliarmus )
	   MessengerFour ! expelliarmus
	 }
	 override def handle( response : AJustStringResponse ) = {
	   //println( "handling: " + response )
	   response match {
	     case JustifiedResponse(
	       mgc, _,
	       respondersName,
	       color,
	       content, Some( req )
	     ) => {
	       //println( "matched response case!" )
	       respondersName.getFragment match {
		 case "Ron" => {
		   //println( "matched Ron case!" )
		   throw new Exception( "shouldn't get here!" )
		 }
		 case "Herminone" => {
		   // traceMonitor ! CloseSession( MessengerOne )
		   // traceMonitor ! LogMessage( this, "Should close log" )
		   //println( "matched Herminone case!" )
		   //println( "closing monitor session" )		   
		   // println(
// 		     "session status : " + traceMonitor.sessionStatus( this )
// 		   )
		   responsesReceived.length match {
		     case 1 => {
		       responsesReceived += response
		       traceMonitor.closeMonitoringSession( this )
		     }
		     case _ => {
		       responsesReceived += response
		       traceMonitor.traceEvent(
			 this,
			 "<waiting>Draco " + responses.length + "</waiting>"
		       )
		     }
		   }
		 }
		 case "Draco" => {
		   //println( "matched Draco case!" )
		   //throw new Exception( "shouldn't get here!" )
		   responsesReceived.length match {
		     case 1 => {
		       responsesReceived += response
		       traceMonitor.closeMonitoringSession( this )
		     }
		     case _ => {
		       responsesReceived += response
		       // traceMonitor.traceEvent(
// 			 this,
// 			 "<waiting>Hermione " + responses.length + "</waiting>"
// 		       )
		     }
		   }
		 }
		 case x => {
		   //println( "unexpected name" + x )
		   throw new Exception( "shouldn't get here!" )		   
		 }
	       }
	     }
	     case _ => {
	       //println( "matched exception case!" )
	       throw new Exception( "That's no way to talk to a wizard!" )
	     }
	   }
	   true
	 }
       }

  case object MessengerTwo
     extends Messenger[String,String](
       ronsName,
       new ListBuffer[AJustStringRequest](),
       new ListBuffer[AJustStringResponse](),
       Some( new LinkedHashMap[URI,Socialite[String,String]]() ),
       AStringTraceMonitor
       ) {
	 override def useBraceNotation : Boolean = true
	 def script() = {
	   markRequest( pickUpLine )
	   MessengerThree ! pickUpLine
	 }
	 override def handle( response : AJustStringResponse ) = {
	   //println( "handling: " + response )
	   response match {
	     case JustifiedResponse(
	       mgc, _,
	       respondersName,
	       color,
	       content, Some( req )
	     ) => {
	       respondersName.getFragment match {
		 case "Harry" => {
		   throw new Exception( "shouldn't get here!" )
		 }
		 case "Herminone" => {
		   // traceMonitor ! CloseSession( MessengerTwo )
		   // traceMonitor ! LogMessage( this, "Should close log" )
		   //println( "closing monitor session" )
		   traceMonitor.closeMonitoringSession( this )
		   // println(
// 		     "session status : " + traceMonitor.sessionStatus( this )
// 		   )
		 }
		 case "Draco" => {
		   throw new Exception( "shouldn't get here!" )
		 }
	       }
	     }
	     case _ => {
	       throw new Exception( "That's no way to talk to a wizard!" )
	     }
	   }
	   true
	 }
       }

  case object MessengerThree
     extends Messenger[String,String](
       hermionesName,
       new ListBuffer[AJustStringRequest](),
       new ListBuffer[AJustStringResponse](),
       Some( new LinkedHashMap[URI,Socialite[String,String]]() ),
       AStringTraceMonitor
       ) {
	 override def useBraceNotation : Boolean = true
	 def script() = {	   
	   //markRequest( pickUpLine )
	   //MessengerThree ! pickUpLine
	 }
	 override def handle( request : AJustStringRequest ) = {
	   //println( "handling: " + request )
	   request match {
	     case JustifiedRequest(
	       mgc, _,
	       suitorsName,
	       color,
	       content, None
	     ) => {
	       suitorsName.getFragment match {
		 case "Ron" => {
		   //println( "case Ron" )
		   markResponse( flirt )
		   MessengerTwo ! flirt
		   responses.length match {
		     case 2 => {
		       // traceMonitor ! CloseSession( MessengerThree )
		       // traceMonitor ! LogMessage( this, "Should close log" )	
		       //println( "closing monitor session" )
		       traceMonitor.closeMonitoringSession( this )
		       // println(
// 			 "session status : " + traceMonitor.sessionStatus( this )
// 		       )
		     }
		     case _ => {
		       traceMonitor.traceEvent(
			 this,
			 "<waiting>Harry " + responses.length + "</waiting>"
		       )
		     }
		   }
		 }
		 case "Harry" => {
		   //println( "case Harry" )
		   markResponse( friendlyResponse )
		   MessengerOne ! friendlyResponse
		   responses.length match {
		     case 2 => {
		       //traceMonitor ! CloseSession( MessengerThree )
		       //traceMonitor.traceEvent( this, "Should close log" )
		       //println( "closing monitor session" )
		       traceMonitor.closeMonitoringSession( this )
		       // println(
// 			 "session status : " + traceMonitor.sessionStatus( this )
// 		       )
		     }
		     case _ => {
		       traceMonitor.traceEvent(
			 this,
			 "<waiting>Ron " + responses.length + "</waiting>"
		       )
		     }
		   }
		 }
	       }
	     }
	     case _ => {
	       //println( "case got some other kind of message" )
	       throw new Exception( "That's no way to talk to a witch!" )
	     }
	   }
	   true
	 }
       }

  case object MessengerFour
     extends Messenger[String,String](
       dracosName,
       new ListBuffer[AJustStringRequest](),
       new ListBuffer[AJustStringResponse](),
       Some( new LinkedHashMap[URI,Socialite[String,String]]() ),
       AStringTraceMonitor
       ) {
	 override def useBraceNotation : Boolean = true
	 def script() = {
	   //markRequest( pickUpLine )
	   //MessengerThree ! pickUpLine
	 }
	 override def handle( request : AJustStringRequest ) = {
	   println( "handling: " + request )
	   request match {
	     case JustifiedRequest(
	       mgc, _,
	       requestersName,
	       color,
	       content, None
	     ) => {
	       requestersName.getFragment match {
		 case "Harry" => {
		   //traceMonitor ! CloseSession( MessengerFour )
		   //traceMonitor.traceEvent( this, "Should close log" )
		   println( "matched Harry case" )
		   markResponse( finiteIncantatem )
		   MessengerOne ! finiteIncantatem
		   traceMonitor.closeMonitoringSession( this )
		   // println(
// 		     "session status : " + traceMonitor.sessionStatus( this )
// 		   )
		 }
		 case "Herminone" => {
		   //traceMonitor ! CloseSession( MessengerFour )
		   //traceMonitor.traceEvent( this, "Should close log" )
		   //println( "closing monitor session" )		   
		   traceMonitor.closeMonitoringSession( this )
		   // println(
// 		     "session status : " + traceMonitor.sessionStatus( this )
// 		   )
		 }
		 case "Ron" => {
		   //traceMonitor ! CloseSession( MessengerFour )
		   //traceMonitor.traceEvent( this, "Should close log" )
		   //println( "closing monitor session" )
		   traceMonitor.closeMonitoringSession( this )
		   // println(
// 		     "session status : " + traceMonitor.sessionStatus( this )
// 		   )
		 }
	       }
	     }
	     case _ => {
	       //println( "Draco exception case" )
	       throw new Exception( "That's no way to talk to a wizard!" )
	     }
	   }
	   true
	 }
       }

  case object RMessengerOne
     extends RMessenger[String,String](
       harrysName,
       new ListBuffer[AJustStringRequest](),
       new ListBuffer[AJustStringResponse](),
       Some( new LinkedHashMap[URI,Socialite[String,String]]() ),
       AStringTraceMonitor
       ) {
	 val responsesReceived : ListBuffer[AJustStringResponse] =
	   new ListBuffer[AJustStringResponse]()
	 override def useBraceNotation : Boolean = true
	 def script() = {	   
	   markRequest( friendlyOverture )
	   RMessengerThree ! friendlyOverture
	   markRequest( expelliarmus )
	   RMessengerFour ! expelliarmus
	 }
	 override def handleWithContinuation(
	   response : AJustStringResponse,
	   k : Status[AJustStringResponse] => Status[AJustStringResponse]
	 ) = {
	   //println( "handling: " + response )
	   response match {
	     case JustifiedResponse(
	       mgc, _,
	       respondersName,
	       color,
	       content, Some( req )
	     ) => {
	       //println( "matched response case!" )
	       respondersName.getFragment match {
		 case "Ron" => {
		   //println( "matched Ron case!" )
		   //throw new Exception( "shouldn't get here!" )
		   k(
		     JRspStatus(
		       response,
		       false,
		       Some( "Identity fraud" ),
		       Some( k )
		     )
		   )
		 }
		 case "Herminone" => {
		   // traceMonitor ! CloseSession( RMessengerOne )
		   // traceMonitor ! LogMessage( this, "Should close log" )
		   //println( "matched Herminone case!" )
		   //println( "closing monitor session" )		   
		   // println(
// 		     "session status : " + traceMonitor.sessionStatus( this )
// 		   )
		   responsesReceived.length match {
		     case 1 => {
		       responsesReceived += response
		       traceMonitor.closeMonitoringSession( this )
		     }
		     case _ => {
		       responsesReceived += response
		       traceMonitor.traceEvent(
			 this,
			 "<waiting>Draco " + responses.length + "</waiting>"
		       )
		     }
		   }
		 }
		 case "Draco" => {
		   //println( "matched Draco case!" )
		   //throw new Exception( "shouldn't get here!" )
		   responsesReceived.length match {
		     case 1 => {
		       responsesReceived += response
		       traceMonitor.closeMonitoringSession( this )
		     }
		     case _ => {
		       responsesReceived += response
		       // traceMonitor.traceEvent(
// 			 this,
// 			 "<waiting>Hermione " + responses.length + "</waiting>"
// 		       )
		     }
		   }
		 }
		 case x => {
		   //println( "unexpected name" + x )
		   //throw new Exception( "shouldn't get here!" )
		   k(
		     JRspStatus(
		       response,
		       false,
		       Some( "Identity fraud" ),
		       Some( k )
		     )
		   )
		 }
	       }
	     }
	     case _ => {
	       //println( "matched exception case!" )
	       //throw new Exception( "That's no way to talk to a wizard!" )
	       k(
		 JRspStatus(
		   response,
		   false,
		   Some( "Unexpected message" ),
		   Some( k )
		 )
	       )
	     }
	   }
	   JRspStatus(
	     response,
	     true,
	     None,
	     Some( k )
	   )
	 }
       }

  case object RMessengerTwo
     extends RMessenger[String,String](
       ronsName,
       new ListBuffer[AJustStringRequest](),
       new ListBuffer[AJustStringResponse](),
       Some( new LinkedHashMap[URI,Socialite[String,String]]() ),
       AStringTraceMonitor
       ) {
	 override def useBraceNotation : Boolean = true
	 def script() = {
	   markRequest( pickUpLine )
	   RMessengerThree ! pickUpLine
	 }
	 override def handleWithContinuation(
	   response : AJustStringResponse,
	   k : Status[AJustStringResponse] => Status[AJustStringResponse]
	 ) = {
	   //println( "handling: " + response )
	   response match {
	     case JustifiedResponse(
	       mgc, _,
	       respondersName,
	       color,
	       content, Some( req )
	     ) => {
	       respondersName.getFragment match {
		 case "Harry" => {
		   //throw new Exception( "shouldn't get here!" )
		   k(
		     JRspStatus(
		       response,
		       false,
		       Some( "Identity fraud" ),
		       Some( k )
		     )
		   )
		 }
		 case "Herminone" => {
		   // traceMonitor ! CloseSession( RMessengerTwo )
		   // traceMonitor ! LogMessage( this, "Should close log" )
		   //println( "closing monitor session" )
		   traceMonitor.closeMonitoringSession( this )
		   // println(
// 		     "session status : " + traceMonitor.sessionStatus( this )
// 		   )
		 }
		 case "Draco" => {
		   //throw new Exception( "shouldn't get here!" )
		   k(
		     JRspStatus(
		       response,
		       false,
		       Some( "Identity fraud" ),
		       Some( k )
		     )
		   )
		 }
	       }
	     }
	     case _ => {
	       //throw new Exception( "That's no way to talk to a wizard!" )
	       k(
		 JRspStatus(
		   response,
		   false,
		   Some( "Unexpected message" ),
		   Some( k )
		 )
	       )
	     }
	   }
	   JRspStatus(
	     response,
	     true,
	     None,
	     Some( k )
	   )
	 }
       }

  case object RMessengerThree
     extends RMessenger[String,String](
       hermionesName,
       new ListBuffer[AJustStringRequest](),
       new ListBuffer[AJustStringResponse](),
       Some( new LinkedHashMap[URI,Socialite[String,String]]() ),
       AStringTraceMonitor
       ) {
	 override def useBraceNotation : Boolean = true
	 def script() = {	   
	   //markRequest( pickUpLine )
	   //RMessengerThree ! pickUpLine
	 }
	 override def handleWithContinuation(
	   request : AJustStringRequest,
	   k : Status[AJustStringRequest] => Status[AJustStringRequest]
	 ) = {
	   //println( "handling: " + request )
	   request match {
	     case JustifiedRequest(
	       mgc, _,
	       suitorsName,
	       color,
	       content, None
	     ) => {
	       suitorsName.getFragment match {
		 case "Ron" => {
		   //println( "case Ron" )
		   markResponse( flirt )
		   RMessengerTwo ! flirt
		   responses.length match {
		     case 2 => {
		       // traceMonitor ! CloseSession( RMessengerThree )
		       // traceMonitor ! LogMessage( this, "Should close log" )	
		       //println( "closing monitor session" )
		       traceMonitor.closeMonitoringSession( this )
		       // println(
// 			 "session status : " + traceMonitor.sessionStatus( this )
// 		       )
		     }
		     case _ => {
		       traceMonitor.traceEvent(
			 this,
			 "<waiting>Harry " + responses.length + "</waiting>"
		       )
		     }
		   }
		 }
		 case "Harry" => {
		   //println( "case Harry" )
		   markResponse( friendlyResponse )
		   RMessengerOne ! friendlyResponse
		   responses.length match {
		     case 2 => {
		       //traceMonitor ! CloseSession( RMessengerThree )
		       //traceMonitor.traceEvent( this, "Should close log" )
		       //println( "closing monitor session" )
		       traceMonitor.closeMonitoringSession( this )
		       // println(
// 			 "session status : " + traceMonitor.sessionStatus( this )
// 		       )
		     }
		     case _ => {
		       traceMonitor.traceEvent(
			 this,
			 "<waiting>Ron " + responses.length + "</waiting>"
		       )
		     }
		   }
		 }
	       }
	     }
	     case _ => {
	       //println( "case got some other kind of message" )
	       //throw new Exception( "That's no way to talk to a witch!" )
	       k(
		 JReqStatus(
		   request,
		   false,
		   Some( "Unexpected message" ),
		   Some( k )
		 )
	       )
	     }
	   }
	   JReqStatus(
	     request,
	     true,
	     None,
	     Some( k )
	   )
	 }
       }

  case object RMessengerFour
     extends RMessenger[String,String](
       dracosName,
       new ListBuffer[AJustStringRequest](),
       new ListBuffer[AJustStringResponse](),
       Some( new LinkedHashMap[URI,Socialite[String,String]]() ),
       AStringTraceMonitor
       ) {
	 override def useBraceNotation : Boolean = true
	 def script() = {
	   //markRequest( pickUpLine )
	   //RMessengerThree ! pickUpLine
	 }
	 override def handleWithContinuation(
	   request : AJustStringRequest,
	   k : Status[AJustStringRequest] => Status[AJustStringRequest]
	 ) = {
	   println( "handling: " + request )
	   request match {
	     case JustifiedRequest(
	       mgc, _,
	       requestersName,
	       color,
	       content, None
	     ) => {
	       requestersName.getFragment match {
		 case "Harry" => {
		   //traceMonitor ! CloseSession( RMessengerFour )
		   //traceMonitor.traceEvent( this, "Should close log" )
		   println( "matched Harry case" )
		   markResponse( finiteIncantatem )
		   RMessengerOne ! finiteIncantatem
		   traceMonitor.closeMonitoringSession( this )
		   // println(
// 		     "session status : " + traceMonitor.sessionStatus( this )
// 		   )
		 }
		 case "Herminone" => {
		   //traceMonitor ! CloseSession( RMessengerFour )
		   //traceMonitor.traceEvent( this, "Should close log" )
		   //println( "closing monitor session" )		   
		   traceMonitor.closeMonitoringSession( this )
		   // println(
// 		     "session status : " + traceMonitor.sessionStatus( this )
// 		   )
		 }
		 case "Ron" => {
		   //traceMonitor ! CloseSession( RMessengerFour )
		   //traceMonitor.traceEvent( this, "Should close log" )
		   //println( "closing monitor session" )
		   traceMonitor.closeMonitoringSession( this )
		   // println(
// 		     "session status : " + traceMonitor.sessionStatus( this )
// 		   )
		 }
	       }
	     }
	     case _ => {
	       //println( "Draco exception case" )
	       //throw new Exception( "That's no way to talk to a wizard!" )
	       k(
		 JReqStatus(
		   request,
		   false,
		   Some( "Unexpected message" ),
		   Some( k )
		 )
	       )
	     }
	   }
	   JReqStatus(
	     request,
	     true,
	     None,
	     Some( k )
	   )
	 }
       }

  def hookup() = {
    // MessengerOne learns his name is Harry
    MessengerOne.introduce( harrysName, MessengerOne )
    // Harry is introduced to MessengerTwo known as Ron
    MessengerOne.introduce( ronsName, MessengerTwo )
    // Harry is introduced to MessengerThree known as Herminone
    MessengerOne.introduce( hermionesName, MessengerThree )
    // Harry is introduced to MessengerFour known as Draco
    MessengerOne.introduce( dracosName, MessengerFour )
    // Connect to log
    AStringTraceMonitor.openMonitoringSession( MessengerOne )

    // MessengerTwo learns his name is Ron
    MessengerTwo.introduce( ronsName, MessengerTwo )
    // Ron is introduced to MessengerOne known as Harry
    MessengerTwo.introduce( harrysName, MessengerOne )
    // Ron is introduced to MessengerThree known as Herminone
    MessengerTwo.introduce( hermionesName, MessengerThree )
    // Ron is introduced to MessengerFour known as Draco
    MessengerTwo.introduce( dracosName, MessengerFour )
    // Connect to log
    AStringTraceMonitor.openMonitoringSession( MessengerTwo )

    // MessengerThree learns her name is Hermione
    MessengerThree.introduce( hermionesName, MessengerThree )
    // Hermione is introduced to MessengerOne known as Harry
    MessengerThree.introduce( harrysName, MessengerOne )
    // Hermione is introduced to MessengerTwo known as Ron
    MessengerThree.introduce( ronsName, MessengerTwo )
    // Hermione is introduced to MessengerFour known as Draco
    MessengerThree.introduce( dracosName, MessengerFour )
    // Connect to log
    AStringTraceMonitor.openMonitoringSession( MessengerThree )

    // MessengerFour learns his name is Draco
    MessengerFour.introduce( dracosName, MessengerFour )
    // Draco is introduced to MessengerOne known as Harry
    MessengerFour.introduce( harrysName, MessengerOne )
    // Draco is introduced to MessengerTwo known as Ron
    MessengerFour.introduce( ronsName, MessengerTwo )
    // Draco is introduced to MessengerThree known as Hermione
    MessengerFour.introduce( hermionesName, MessengerThree )
    // Connect to log
    AStringTraceMonitor.openMonitoringSession( MessengerFour )

    // This community reuses the names for different identities

    // RMessengerOne learns his name is Harry
    RMessengerOne.introduce( harrysName, RMessengerOne )
    // Harry is introduced to RMessengerTwo known as Ron
    RMessengerOne.introduce( ronsName, RMessengerTwo )
    // Harry is introduced to RMessengerThree known as Herminone
    RMessengerOne.introduce( hermionesName, RMessengerThree )
    // Harry is introduced to RMessengerFour known as Draco
    RMessengerOne.introduce( dracosName, RMessengerFour )
    // Connect to log
    AStringTraceMonitor.openMonitoringSession( RMessengerOne )

    // RMessengerTwo learns his name is Ron
    RMessengerTwo.introduce( ronsName, RMessengerTwo )
    // Ron is introduced to RMessengerOne known as Harry
    RMessengerTwo.introduce( harrysName, RMessengerOne )
    // Ron is introduced to RMessengerThree known as Herminone
    RMessengerTwo.introduce( hermionesName, RMessengerThree )
    // Ron is introduced to RMessengerFour known as Draco
    RMessengerTwo.introduce( dracosName, RMessengerFour )
    // Connect to log
    AStringTraceMonitor.openMonitoringSession( RMessengerTwo )

    // RMessengerThree learns her name is Hermione
    RMessengerThree.introduce( hermionesName, RMessengerThree )
    // Hermione is introduced to RMessengerOne known as Harry
    RMessengerThree.introduce( harrysName, RMessengerOne )
    // Hermione is introduced to RMessengerTwo known as Ron
    RMessengerThree.introduce( ronsName, RMessengerTwo )
    // Hermione is introduced to RMessengerFour known as Draco
    RMessengerThree.introduce( dracosName, RMessengerFour )
    // Connect to log
    AStringTraceMonitor.openMonitoringSession( RMessengerThree )

    // RMessengerFour learns his name is Draco
    RMessengerFour.introduce( dracosName, RMessengerFour )
    // Draco is introduced to RMessengerOne known as Harry
    RMessengerFour.introduce( harrysName, RMessengerOne )
    // Draco is introduced to RMessengerTwo known as Ron
    RMessengerFour.introduce( ronsName, RMessengerTwo )
    // Draco is introduced to RMessengerThree known as Hermione
    RMessengerFour.introduce( hermionesName, RMessengerThree )
    // Connect to log
    AStringTraceMonitor.openMonitoringSession( RMessengerFour )
  }

  def activate() = {
    MessengerOne.start
    MessengerTwo.start
    MessengerThree.start
    MessengerFour.start

    RMessengerOne.start
    RMessengerTwo.start
    RMessengerThree.start
    RMessengerFour.start

    AStringTraceMonitor.start
  }

  def mix() = {                
    MessengerOne.script()
    MessengerTwo.script()
    MessengerThree.script()
    MessengerFour.script()

    RMessengerOne.script()
    RMessengerTwo.script()
    RMessengerThree.script()
    RMessengerFour.script()
    // traceMonitor ! ShowLog()
  }
}




