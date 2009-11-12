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

trait Header {
  def msgId         : UUID
  def to            : URI
  def from          : URI
  def flowId        : UUID
}

trait Message[Justfication,BodyType] {  
  def body          : BodyType
  def justification : Option[Justfication]  
}

abstract class Request[Response,BodyType](
  msgId         : UUID,
  to            : URI,
  from          : URI,
  flowId        : UUID,
  body          : BodyType,
  justification : Option[Response]  
) extends Message[Response,BodyType] with Header

abstract class AbstractJustifiedRequest[ReqBody,RspBody](
  msgId         : UUID,
  to            : URI,
  from          : URI,
  flowId        : UUID,
  body          : ReqBody,
  justification : Option[Response[AbstractJustifiedRequest[ReqBody,RspBody],RspBody]]
) extends Request[Response[AbstractJustifiedRequest[ReqBody,RspBody],RspBody],ReqBody](
  msgId, to, from, flowId, body, justification
)

case class JustifiedRequest[ReqBody,RspBody](
  msgId         : UUID,
  to            : URI,
  from          : URI,
  flowId        : UUID,
  body          : ReqBody,
  justification : Option[Response[AbstractJustifiedRequest[ReqBody,RspBody],RspBody]]
) extends AbstractJustifiedRequest[ReqBody,RspBody](
  msgId, to, from, flowId, body, justification
)

abstract class Response[Request,BodyType](
  msgId         : UUID,
  to            : URI,
  from          : URI,
  flowId        : UUID,
  body          : BodyType,
  justification : Option[Request]
) extends Message[Request,BodyType]

abstract class AbstractJustifiedResponse[ReqBody,RspBody](
  msgId         : UUID,
  to            : URI,
  from          : URI,
  flowId        : UUID,
  body          : RspBody,
  justification : Option[Request[AbstractJustifiedResponse[ReqBody,RspBody],ReqBody]]
) extends Response[Request[AbstractJustifiedResponse[ReqBody,RspBody],ReqBody],RspBody](
  msgId, to, from, flowId, body, justification
)

case class JustifiedResponse[ReqBody,RspBody](
  msgId         : UUID,
  to            : URI,
  from          : URI,
  flowId        : UUID,
  body          : RspBody,
  justification : Option[Request[AbstractJustifiedResponse[ReqBody,RspBody],ReqBody]]
) extends AbstractJustifiedResponse[ReqBody,RspBody](
  msgId, to, from, flowId, body, justification
)

// Inspection and other control plane messages

trait InspectionRequest extends Message[Unit,Unit] with Header {
  val flowId : UUID = new UUID()
}

case class InspectRequests( to : URI, from : URI )
     extends InspectionRequest {
       override def msgId = new UUID()
       override def body = {}
       override def justification = None       
     }

case class InspectResponses( to : URI, from : URI )
     extends InspectionRequest {
       override def msgId = new UUID()
       override def body = {}
       override def justification = None
     }

case class InspectNamespace( to : URI, from : URI )
     extends InspectionRequest {
       override def msgId = new UUID()
       override def body = {}
       override def justification = None
     }

trait InspectionResponse[Request] extends Message[Request,Unit]
