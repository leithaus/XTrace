// -*- mode: Scala;-*- 
// Filename:    Magic.scala 
// Authors:     lgm                                                    
// Creation:    Mon Nov 16 10:58:26 2009 
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

trait Hogwarts {
  val hogwartsTrueName : UUID = new UUID()

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
  val attraction        : UUID = new UUID()
  val friendship        : UUID = new UUID()
  val magicalAttack     : UUID = new UUID()
  val magicalAttackTwo  : UUID = new UUID()
  val magicalDefense    : UUID = new UUID()
  val magicalDefenseTwo : UUID = new UUID()

  // Messages

  type AJustStringRequest = JustifiedRequest[String,String]
  type AJustStringResponse = JustifiedResponse[String,String]  

  val pickUpLine : AJustStringRequest =
    JustifiedRequest(
      new UUID(),
      hermionesName,
      ronsName,
      attraction,
      "chocolateFrog?",
      None
    )
  val friendlyOverture : AJustStringRequest =
    JustifiedRequest(
      new UUID,
      hermionesName,
      harrysName,
      friendship,
      "bean?",
      None      
    )
  val expelliarmus : AJustStringRequest =
    JustifiedRequest(
      new UUID(),
      dracosName,
      harrysName,
      magicalDefense,
      "expelliarmus",
      None      
    )
  //   val expectoPatronum : AJustStringRequest =
  //     JustifiedRequest( 0, "expectoPatronum", None, dracosName, harrysName )
  val confundo : AJustStringRequest =
    JustifiedRequest(
      new UUID(),
      harrysName,
      dracosName,
      magicalAttack,
      "confundo",
      None      
    )
  val impedimenta : AJustStringRequest =
    JustifiedRequest(
      new UUID(),
      ronsName,
      harrysName,
      magicalAttackTwo,
      "impedimenta",
      None      
    )
  val finiteIncantatem : AJustStringResponse =
    JustifiedResponse(
      new UUID(),
      harrysName,
      dracosName,
      magicalDefenseTwo,
      "finiteIncantatem",
      Some( expelliarmus.asInstanceOf[Request[AbstractJustifiedResponse[String,String],String]] )      
    )

  val flirt : AJustStringResponse =
    JustifiedResponse(
      new UUID(),
      ronsName,
      hermionesName,
      attraction,
      "chocolateFrog!",
      Some( pickUpLine.asInstanceOf[Request[AbstractJustifiedResponse[String,String],String]] )      
    )
  val friendlyResponse : AJustStringResponse =
    JustifiedResponse(
      new UUID(),
      harrysName,
      hermionesName,
      friendship,
      "bean.",
      Some( friendlyOverture.asInstanceOf[Request[AbstractJustifiedResponse[String,String],String]] )      
    )    
}
