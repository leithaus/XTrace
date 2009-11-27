// -*- mode: Scala;-*- 
// Filename:    ChoiceMonitor.scala 
// Authors:     lgm                                                    
// Creation:    Tue Nov 24 12:18:55 2009 
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

class ChoiceMonitor[Task]
extends Actor
with TraceMonitorT[Choice[Task]] {
  var _debuggingLevel : DebuggingLevel = Naked()
  override def debuggingLevel : DebuggingLevel = _debuggingLevel
  override def debuggingLevel( dbglvl : DebuggingLevel ) : Unit = {
    _debuggingLevel = dbglvl
  }
  //var debuggingLevel : DebuggingLevel = FullyClothed()
  val sessions : LinkedHashMap[Choice[Task],SessionStatus[Choice[Task]]] =
    new LinkedHashMap[Choice[Task],SessionStatus[Choice[Task]]]()
  val messageLog : ListBuffer[Report[Choice[Task]]] =
    new ListBuffer[Report[Choice[Task]]]()      
  
  // Be very careful with this interface. Essentially all interaction
  // with a monitor must be a transaction/function-call. Otherwise,
  // you will encounter surprising race conditions.
  def act () {
    receive {
      case OpenSession( agent ) => {
	openMonitoringSession( agent.asInstanceOf[Choice[Task]] )
	act()
      }
      case CloseSession( agent ) => {
	closeMonitoringSession( agent.asInstanceOf[Choice[Task]] )
	act()
      }
    }
  }
}
