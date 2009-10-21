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

trait LogAction
trait Report {
  def agent : Socialite
  def message : String
}
case class TraceEvent( agent : Socialite, msg : String )
     extends LogAction with Report {
       override def message = msg
     }
case class TraceXmlEvent( agent : Socialite, msg : Elem ) 
     extends LogAction with Report {
       override def message = msg.toString
     }
case class ShowLog() extends LogAction

trait SessionStatus
case class OpenSession( agent : Socialite )
     extends LogAction with SessionStatus
case class CloseSession( agent : Socialite )
     extends LogAction with SessionStatus

trait DebuggingLevel 
case class Naked() extends DebuggingLevel
case class FullyClothed() extends DebuggingLevel

class TraceMonitor extends Actor {
  var debuggingLevel : DebuggingLevel = Naked()
  //var debuggingLevel : DebuggingLevel = FullyClothed()
  val sessions : LinkedHashMap[Socialite,SessionStatus] =
    new LinkedHashMap[Socialite,SessionStatus]()
  val messageLog : ListBuffer[Report] =
    new ListBuffer[Report]()

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

  def sessionStatus( agent : Socialite ) : Option[SessionStatus] = {
    sessions.get( agent )
  }
  def openMonitoringSession( agent : Socialite ) : OpenSession = {
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
  def closeMonitoringSession( agent : Socialite ) : CloseSession = {
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
  
  def traceEvent( agent : Socialite, msg : String ) : Unit = {
    traceEvent( TraceEvent( agent, msg ) )
  }

  def traceEvent( report : Report ) : Unit = {
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
	openMonitoringSession( agent )
	act()
      }
      case CloseSession( agent ) => {
	closeMonitoringSession( agent )
	act()
      }
      case TraceEvent( agent, msg ) => {
	traceEvent( TraceEvent( agent, msg ) )
	act()
      }
      case TraceXmlEvent( agent, msg ) => {
	traceEvent( TraceXmlEvent( agent, msg ) )
	act()
      }
      case ShowLog() => {
	showLog()
	act()
      }
    }
  }
}

object ATraceMonitor extends TraceMonitor
