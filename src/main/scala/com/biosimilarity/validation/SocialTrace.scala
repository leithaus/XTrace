// -*- mode: Scala;-*- 
// Filename:    SocialTrace.scala 
// Authors:     lgm                                                    
// Creation:    Tue Nov 24 12:05:33 2009 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.validation

import scala.collection.mutable._
import scala.xml._
import scala.actors._
import Actor._

case class TraceSocialEvent[ReqBody,RspBody]( agent : Socialite[ReqBody,RspBody], msg : String )
     extends LogAction[Socialite[ReqBody,RspBody]]
     with Report[Socialite[ReqBody,RspBody]] {
       override def message = msg
     }
case class TraceSocialXmlEvent[ReqBody,RspBody]( agent : Socialite[ReqBody,RspBody], msg : Elem ) 
     extends LogAction[Socialite[ReqBody,RspBody]]
     with Report[Socialite[ReqBody,RspBody]] {
       override def message = msg.toString
     }
case class ShowLog[ReqBody,RspBody]() extends LogAction[Socialite[ReqBody,RspBody]]

class TraceMonitor[ReqBody,RspBody]
extends Actor
with TraceMonitorT[Socialite[ReqBody,RspBody]] {
  var _debuggingLevel : DebuggingLevel = Naked()
  override def debuggingLevel : DebuggingLevel = _debuggingLevel
  override def debuggingLevel( dbglvl : DebuggingLevel ) : Unit = {
    _debuggingLevel = dbglvl
  }
  //var debuggingLevel : DebuggingLevel = FullyClothed()
  val sessions : LinkedHashMap[Socialite[ReqBody,RspBody],SessionStatus[Socialite[ReqBody,RspBody]]] =
    new LinkedHashMap[Socialite[ReqBody,RspBody],SessionStatus[Socialite[ReqBody,RspBody]]]()
  val messageLog : ListBuffer[Report[Socialite[ReqBody,RspBody]]] =
    new ListBuffer[Report[Socialite[ReqBody,RspBody]]]()    
  
  def traceEvent( agent : Socialite[ReqBody,RspBody], msg : String ) : Unit = {
    traceEvent( TraceSocialEvent( agent, msg ) )
  }
  
  // Be very careful with this interface. Essentially all interaction
  // with a monitor must be a transaction/function-call. Otherwise,
  // you will encounter surprising race conditions.
  def act () {
    receive {
      case OpenSession( agent ) => {
	openMonitoringSession( agent.asInstanceOf[Socialite[ReqBody,RspBody]] )
	act()
      }
      case CloseSession( agent ) => {
	closeMonitoringSession( agent.asInstanceOf[Socialite[ReqBody,RspBody]] )
	act()
      }
      case TraceSocialEvent( agent, msg ) => {
	traceEvent( TraceSocialEvent( agent.asInstanceOf[Socialite[ReqBody,RspBody]], msg ) )
	act()
      }
      case TraceSocialXmlEvent( agent, msg ) => {
	traceEvent( TraceSocialXmlEvent( agent.asInstanceOf[Socialite[ReqBody,RspBody]], msg ) )
	act()
      }
      case ShowLog() => {
	showLog()
	act()
      }
    }
  }
}

object AStringTraceMonitor extends TraceMonitor[String,String]
