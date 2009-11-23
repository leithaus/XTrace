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

object Cabal extends Community[String,String] with Hogwarts {    
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
  
  override def identity = hogwartsTrueName
  
  override def roles = {
    List(
      harrysName,
      ronsName,
      hermionesName,
      dracosName
    )
  }

  override def population = {
    List(
      MessengerOne,
      MessengerTwo,
      MessengerThree,
      MessengerFour
    )
  }  

  override def hookup() = {
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
  }

  override def activate() = {
    MessengerOne.start
    MessengerTwo.start
    MessengerThree.start
    MessengerFour.start

    AStringTraceMonitor.start
  }

  override def mix() = {                
    MessengerOne.script()
    MessengerTwo.script()
    MessengerThree.script()
    MessengerFour.script()
  }
}




