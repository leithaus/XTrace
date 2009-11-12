// -*- mode: Scala;-*- 
// Filename:    SimpleMessage.scala 
// Authors:     lgm                                                    
// Creation:    Tue Nov 10 19:51:10 2009 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.validation

import java.net.URI
import com.eaio.uuid.UUID

case class JustStrRequest(
  msgId         : UUID,
  to            : URI,
  from          : URI,
  flowId        : UUID,
  body          : String,
  justification : Option[Response[AbstractJustifiedRequest[String,String],String]]
) extends AbstractJustifiedRequest[String,String](
  msgId, to, from, flowId, body, justification
)

case class JustStrResponse(
  msgId         : UUID,
  to            : URI,
  from          : URI,
  flowId        : UUID,
  body          : String,
  justification : Option[Request[AbstractJustifiedResponse[String,String],String]]
) extends AbstractJustifiedResponse[String,String](
  msgId, to, from, flowId, body, justification
)

// case class JustCnxnMgrMsg(
//   msgId         : UUID,
//   to            : URI,
//   from          : URI,
//   flowId        : UUID,
//   body          : ConnectionManagerMsg,
//   justification : Option[Request[JustifiedResponse[ConnectionManagerMsg,ConnectionManagerMsg],ConnectionManagerMsg]]
// ) extends JustifiedRequest[ConnectionManagerMsg,ConnectionManagerMsg](
//   msgId, to, from, flowId, body, justification
// )
