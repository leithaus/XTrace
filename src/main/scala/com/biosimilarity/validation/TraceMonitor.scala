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

trait LogAction[Client]
trait Report[Client] {
  def agent : Client
  def message : String
}
trait SessionStatus[Client]
case class OpenSession[Client]( agent : Client )
     extends LogAction[Client]
     with SessionStatus[Client]
case class CloseSession[Client]( agent : Client )
     extends LogAction[Client]
     with SessionStatus[Client]

trait DebuggingLevel 
case class Naked() extends DebuggingLevel
case class FullyClothed() extends DebuggingLevel

trait TraceMonitorT[Client] extends {
  def debuggingLevel : DebuggingLevel
  def debuggingLevel( dbglvl : DebuggingLevel ) : Unit

  def sessions : LinkedHashMap[Client,SessionStatus[Client]]
  def messageLog : ListBuffer[Report[Client]]

  def dumpLogToString : String = dumpLogToWriter( new java.io.StringWriter() )
  def dumpLogToXML : Sequence[Node] = {
    messageLog.map(
      {
	case r : Report[Client] => {
	  <entry>{r.message}</entry>
	}
      }
    )
  }
  def dumpLogToWriter( sw : java.io.StringWriter ) : String = {
    messageLog.map( { case r : Report[Client] => sw.write( r.message ) } )
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

  def sessionStatus( agent : Client )
  : Option[SessionStatus[Client]] = {
    sessions.get( agent )
  }
  def openMonitoringSession( agent : Client )
  : OpenSession[Client] = {
    sessions.get( agent ) match {
      case Some( agent ) => {
	throw new Exception( "already in session" )
      }
      case None => {
	val session = OpenSession[Client]( agent )
	sessions.update( agent, session )
	session
      }
    }
  }
  def closeMonitoringSession( agent : Client )
  : CloseSession[Client] = {
    sessions.get( agent ) match {
      case Some( session ) => {
	val session = CloseSession[Client]( agent )
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

  def traceEvent( report : Report[Client] ) : Unit = {
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
	debuggingLevel match {
	  case Naked() => {
	    println(
	      (
		"logging event to console: "
		+ "Monitoring against a closed session!"
	      )
	    )
	  }
	}
	throw new Exception( "Monitoring against a closed session!" )
      }
      case None => {
	debuggingLevel match {
	  case Naked() => {
	    println(
	      (
		"logging event to console: "
		+ "Monitoring without opening a session"
	      )
	    )
	  }
	}
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
    
}


class SimpleMonitor[Client]
extends Actor
with TraceMonitorT[Client] {
  var _debuggingLevel : DebuggingLevel = Naked()
  override def debuggingLevel : DebuggingLevel = _debuggingLevel
  override def debuggingLevel( dbglvl : DebuggingLevel ) : Unit = {
    _debuggingLevel = dbglvl
  }
  //var debuggingLevel : DebuggingLevel = FullyClothed()
  val sessions : LinkedHashMap[Client,SessionStatus[Client]] =
    new LinkedHashMap[Client,SessionStatus[Client]]()
  val messageLog : ListBuffer[Report[Client]] =
    new ListBuffer[Report[Client]]()      
  
  // Be very careful with this interface. Essentially all interaction
  // with a monitor must be a transaction/function-call. Otherwise,
  // you will encounter surprising race conditions.
  def act () {
    receive {
      case OpenSession( agent ) => {
	openMonitoringSession( agent.asInstanceOf[Client] )
	act()
      }
      case CloseSession( agent ) => {
	closeMonitoringSession( agent.asInstanceOf[Client] )
	act()
      }
    }
  }
}
