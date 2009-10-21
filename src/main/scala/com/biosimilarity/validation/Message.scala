// -*- mode: Scala;-*- 
// Filename:    TheMessage.scala 
// Authors:     lgm                                                    
// Creation:    Fri Oct 16 14:18:12 2009 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.validation

import java.net.URI
import com.eaio.uuid.UUID

trait Message[Justfication] {
  def magic : Int
  def body : String
  def deeper : Option[Justfication]
  def to : URI
  def from : URI
  def color : UUID
}

abstract class Request[Response](
  magic : Int,
  body : String,
  deeper : Option[Response],
  to : URI,
  from : URI,
  color : UUID
) extends Message[Response]

trait InspectionRequest extends Message[Unit] {
  val color : UUID = new UUID()
}

case class JustifiedRequest(
  magic : Int,
  body : String,
  deeper : Option[Response[JustifiedRequest]],
  to : URI,
  from : URI,
  color : UUID
) extends Request[Response[JustifiedRequest]](
  magic, body, deeper, to, from, color
)

case class InspectRequests( to : URI, from : URI )
     extends InspectionRequest {
       override def magic = 1001
       override def body = "InspectRequests"
       override def deeper = None       
     }

case class InspectResponses( to : URI, from : URI )
     extends InspectionRequest {
       override def magic = 1002
       override def body = "InspectResponses"
       override def deeper = None
     }

case class InspectNamespace( to : URI, from : URI )
     extends InspectionRequest {
       override def magic = 1003
       override def body = "InspectNamespace"
       override def deeper = None
     }

abstract class Response[Request](
  magic : Int,
  body : String,
  deeper : Option[Request],
  to : URI,
  from : URI,
  color : UUID
) extends Message[Request]

trait InspectionResponse[Request] extends Message[Request]

case class JustifiedResponse(
  magic : Int,
  body : String,
  deeper : Option[Request[JustifiedResponse]],
  to : URI,
  from : URI,
  color : UUID
) extends Response[Request[JustifiedResponse]](
  magic, body, deeper, to, from, color
)





