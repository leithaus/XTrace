// -*- mode: Scala;-*- 
// Filename:    WorkMonitor.scala 
// Authors:     lgm                                                    
// Creation:    Mon Nov 30 09:44:43 2009 
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

case class TraceWorkEvent[Task]( agent : WorkLog[Task], msg : String )
     extends LogAction[WorkLog[Task]]
     with Report[WorkLog[Task]] {
       override def message = msg
     }

class WorkMonitor[Task] extends SimpleMonitor[WorkLog[Task]] {
  def traceEvent( agent : WorkLog[Task], msg : String ) : Unit = {
    traceEvent( TraceWorkEvent( agent, msg ) )
  }
}
