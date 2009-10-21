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

  val pickUpLine : JustifiedRequest =
    JustifiedRequest(
      0,
      "chocolateFrog?",
      None,
      hermionesName,
      ronsName,
      attraction
    )
  val friendlyOverture : JustifiedRequest =
    JustifiedRequest(
      0,
      "bean?",
      None,
      hermionesName,
      harrysName,
      friendship
    )
  val expelliarmus : JustifiedRequest =
    JustifiedRequest(
      0,
      "expelliarmus",
      None,
      dracosName,
      harrysName,
      magicalDefense
    )
//   val expectoPatronum : JustifiedRequest =
//     JustifiedRequest( 0, "expectoPatronum", None, dracosName, harrysName )
  val confundo : JustifiedRequest =
    JustifiedRequest(
      0,
      "confundo",
      None,
      harrysName,
      dracosName,
      magicalAttack
    )
  val impedimenta : JustifiedRequest =
    JustifiedRequest(
      0,
      "impedimenta",
      None,
      ronsName,
      harrysName,
      magicalAttackTwo
    )
  val finiteIncantatem : JustifiedResponse =
    JustifiedResponse(
      0,
      "finiteIncantatem",
      Some( expelliarmus.asInstanceOf[Request[JustifiedResponse]] ),
      harrysName,
      dracosName,
      magicalDefenseTwo
    )

  val flirt : JustifiedResponse =
    JustifiedResponse(
      1,
      "chocolateFrog!",
      Some( pickUpLine.asInstanceOf[Request[JustifiedResponse]] ),
      ronsName,
      hermionesName,
      attraction
    )
  val friendlyResponse : JustifiedResponse =
    JustifiedResponse(
      1,
      "bean.",
      Some( friendlyOverture.asInstanceOf[Request[JustifiedResponse]] ),
      harrysName,
      hermionesName,
      friendship
    )  

  case object MessengerOne
     extends Messenger(
       harrysName,
       new ListBuffer[JustifiedRequest](),
       new ListBuffer[JustifiedResponse](),
       Some( new LinkedHashMap[URI,Socialite]() ),
       ATraceMonitor
       ) {
	 val responsesReceived : ListBuffer[JustifiedResponse] =
	   new ListBuffer[JustifiedResponse]()
	 override def useBraceNotation : Boolean = true
	 def script() = {	   
	   markRequest( friendlyOverture )
	   MessengerThree ! friendlyOverture
	   markRequest( expelliarmus )
	   MessengerFour ! expelliarmus
	 }
	 override def handle( response : JustifiedResponse ) = {
	   //println( "handling: " + response )
	   response match {
	     case JustifiedResponse(
	       mgc, content, Some( req ),
	       _,
	       respondersName,
	       color
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
		       // traceMonitor.traceEvent(
// 			 this,
// 			 "<waiting>Draco " + responses.length + "</waiting>"
// 		       )
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
     extends Messenger(
       ronsName,
       new ListBuffer[JustifiedRequest](),
       new ListBuffer[JustifiedResponse](),
       Some( new LinkedHashMap[URI,Socialite]() ),
       ATraceMonitor
       ) {
	 override def useBraceNotation : Boolean = true
	 def script() = {
	   markRequest( pickUpLine )
	   MessengerThree ! pickUpLine
	 }
	 override def handle( response : JustifiedResponse ) = {
	   //println( "handling: " + response )
	   response match {
	     case JustifiedResponse(
	       mgc, content, Some( req ),
	       _,
	       respondersName,
	       color
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
     extends Messenger(
       hermionesName,
       new ListBuffer[JustifiedRequest](),
       new ListBuffer[JustifiedResponse](),
       Some( new LinkedHashMap[URI,Socialite]() ),
       ATraceMonitor
       ) {
	 override def useBraceNotation : Boolean = true
	 def script() = {	   
	   //markRequest( pickUpLine )
	   //MessengerThree ! pickUpLine
	 }
	 override def handle( request : JustifiedRequest ) = {
	   //println( "handling: " + request )
	   request match {
	     case JustifiedRequest(
	       0, content, None,
	       _,
	       suitorsName,
	       color
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
     extends Messenger(
       dracosName,
       new ListBuffer[JustifiedRequest](),
       new ListBuffer[JustifiedResponse](),
       Some( new LinkedHashMap[URI,Socialite]() ),
       ATraceMonitor
       ) {
	 override def useBraceNotation : Boolean = true
	 def script() = {
	   //markRequest( pickUpLine )
	   //MessengerThree ! pickUpLine
	 }
	 override def handle( request : JustifiedRequest ) = {
	   println( "handling: " + request )
	   request match {
	     case JustifiedRequest(
	       mgc, content, None,
	       _,
	       requestersName,
	       color
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
    ATraceMonitor.openMonitoringSession( MessengerOne )

    // MessengerTwo learns his name is Ron
    MessengerTwo.introduce( ronsName, MessengerTwo )
    // Ron is introduced to MessengerOne known as Harry
    MessengerTwo.introduce( harrysName, MessengerOne )
    // Ron is introduced to MessengerThree known as Herminone
    MessengerTwo.introduce( hermionesName, MessengerThree )
    // Ron is introduced to MessengerFour known as Draco
    MessengerTwo.introduce( dracosName, MessengerFour )
    // Connect to log
    ATraceMonitor.openMonitoringSession( MessengerTwo )

    // MessengerThree learns her name is Hermione
    MessengerThree.introduce( hermionesName, MessengerThree )
    // Hermione is introduced to MessengerOne known as Harry
    MessengerThree.introduce( harrysName, MessengerOne )
    // Hermione is introduced to MessengerTwo known as Ron
    MessengerThree.introduce( ronsName, MessengerTwo )
    // Hermione is introduced to MessengerFour known as Draco
    MessengerThree.introduce( dracosName, MessengerFour )
    // Connect to log
    ATraceMonitor.openMonitoringSession( MessengerThree )

    // MessengerFour learns his name is Draco
    MessengerFour.introduce( dracosName, MessengerFour )
    // Draco is introduced to MessengerOne known as Harry
    MessengerFour.introduce( harrysName, MessengerOne )
    // Draco is introduced to MessengerTwo known as Ron
    MessengerFour.introduce( ronsName, MessengerTwo )
    // Draco is introduced to MessengerThree known as Hermione
    MessengerFour.introduce( hermionesName, MessengerThree )
    // Connect to log
    ATraceMonitor.openMonitoringSession( MessengerFour )
  }

  def activate() = {
    MessengerOne.start
    MessengerTwo.start
    MessengerThree.start
    MessengerFour.start
    ATraceMonitor.start
  }

  def mix() = {                
    MessengerOne.script()
    MessengerTwo.script()
    MessengerThree.script()
    MessengerFour.script()
    // traceMonitor ! ShowLog()
  }
}




