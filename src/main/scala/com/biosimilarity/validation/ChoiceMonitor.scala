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

class ChoiceMonitor[Task] extends SimpleMonitor[Choice[Task]]
