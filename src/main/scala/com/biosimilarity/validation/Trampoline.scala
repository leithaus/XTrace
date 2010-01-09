// -*- mode: Scala;-*- 
// Filename:    Trampoline.scala 
// Authors:     lgm                                                    
// Creation:    Wed Jan  6 11:25:37 2010 
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

import java.lang.reflect.Field
import java.lang.reflect.Method

trait ChannelMap[B <: Actor] {
  def getChannel( uri : URI ) : Option[B]
}

trait Environment[B <: Actor] {
  def nameSpace : Option[LinkedHashMap[URI,B]]  
}

trait Medium[B <: Actor]
 extends ChannelMap[B]
  with Environment[B] {
    override def getChannel( uri : URI ) : Option[B] = {
      nameSpace match {
	case None => None
	case Some( nameMap ) => {
	  nameMap.get( uri )
	}
      }
    }    
  }

trait MonitoredDevice[
  ReqBody <: {def getClass : Class[_]},
  RspBody <: {def getClass : Class[_]}
] {
  def traceMonitor : TraceMonitor[ReqBody,RspBody]
}

trait RepresentativeMedium[
  ReqBody <: {def getClass : Class[_]},
  RspBody <: {def getClass : Class[_]}
] extends Medium[Representation[ReqBody,RspBody]] {  
  self : MonitoredDevice[ReqBody,RspBody] =>
    def place( treqs : List[ReqBody], trsps : List[RspBody] )
  ( uri : URI, handler : Object ) = {
    for( nameMap <- nameSpace ) {
      nameMap.update(
	uri, 
	Representation(
	  handler,
	  TerritorialCartographer( handler, treqs, trsps ),
	  uri,
	  new ListBuffer[JustifiedRequest[ReqBody,RspBody]](),
	  new ListBuffer[JustifiedResponse[ReqBody,RspBody]](),
	  nameSpace.asInstanceOf[Option[scala.collection.mutable.LinkedHashMap[java.net.URI,com.biosimilarity.validation.Socialite[ReqBody,RspBody]]]],
	  traceMonitor
	)
      )
    }
  }
}

case class MonitoredRepresentativeMedium[
  ReqBody <: {def getClass : Class[_]},
  RspBody <: {def getClass : Class[_]}
](
  nameSpace    : Option[LinkedHashMap[URI,Representation[ReqBody,RspBody]]],
  traceMonitor : TraceMonitor[ReqBody,RspBody]
) extends RepresentativeMedium[ReqBody,RspBody]
  with MonitoredDevice[ReqBody,RspBody]
{
}

trait Community[ReqBody,RspBody] {
  // Structural relationships
  def identity   : UUID
  def roles      : List[URI]
  def population : List[Socialite[ReqBody,RspBody]]
  def hookup()   : Unit 
  // Evaluation behavior
  def activate() : Unit
  def mix()      : Unit
}
