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

case class TraceWorkEvent[Task]( agent : WorkManager[Task], msg : String )
     extends LogAction[WorkManager[Task]]
     with Report[WorkManager[Task]] {
       override def message = msg
     }

class WorkMonitor[Task] extends SimpleMonitor[WorkManager[Task]] {
  def traceEvent( agent : WorkManager[Task], msg : String ) : Unit = {
    traceEvent( TraceWorkEvent( agent, msg ) )
  }
}
