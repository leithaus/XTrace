// -*- mode: Scala;-*- 
// Filename:    Representation.scala 
// Authors:     lgm                                                    
// Creation:    Thu Jan  7 21:59:12 2010 
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

trait Bond {
  def src : Object
  def trgt : Object
}

trait Territory extends Bond {
  def territory : Object
  override def src = territory
  override def trgt = territory
}

trait BondMapping[
  ReqBody <: {def getClass : Class[_]},
  RspBody <: {def getClass : Class[_]}
] extends Bond {  
  def reqMap
  : Option[LinkedHashMap[ReqBody,(Method,Option[LinkedHashMap[Field,Int]])]]
  def rspMap
  : Option[LinkedHashMap[RspBody,(Method,Option[LinkedHashMap[Field,Int]])]]  
}

trait BondMapMaker[
  ReqBody <: {def getClass : Class[_]},
  RspBody <: {def getClass : Class[_]}
] extends Bond {

  def typicalRequests : List[ReqBody]
  def typicalResponses : List[RspBody]

  def stdReqMap
  : Option[LinkedHashMap[ReqBody,(Method,Option[LinkedHashMap[Field,Int]])]]
  = {
    val rslt =
      new LinkedHashMap[ReqBody,(Method,Option[LinkedHashMap[Field,Int]])]()
    
    for( request <- typicalRequests ) {
      for(
	m <- src.getClass.getMethods
	if m.getName == request.getClass.getName
      ) {
	rslt.update( request, ( m, None ) )
      }
    }

    Some( rslt )
  }

  def stdRspMap
  : Option[LinkedHashMap[RspBody,(Method,Option[LinkedHashMap[Field,Int]])]]
  = {
    val rslt =
      new LinkedHashMap[RspBody,(Method,Option[LinkedHashMap[Field,Int]])]()
    
    for( response <- typicalResponses ) {
      for(
	m <- trgt.getClass.getMethods
	if m.getName == response.getClass.getName
      ) {
	rslt.update( response, ( m, None ) )
      }
    }

    Some( rslt )
  }
}

abstract class Cartographer[
  ReqBody <: {def getClass : Class[_]},
  RspBody <: {def getClass : Class[_]}
](
  typicalRequests : List[ReqBody],
  typicalResponses : List[RspBody]
) extends BondMapMaker[ReqBody,RspBody]

case class TerritorialCartographer[
  ReqBody <: {def getClass : Class[_]},
  RspBody <: {def getClass : Class[_]}
](
  territory : Object,
  typicalRequests : List[ReqBody],
  typicalResponses : List[RspBody]
) extends Cartographer[ReqBody,RspBody](
  typicalRequests,
  typicalResponses
) with Territory

trait BondProxy[
  ReqBody <: {def getClass : Class[_]},
  RspBody <: {def getClass : Class[_]}
] extends BondMapping[ReqBody,RspBody] {
  var _reqMap
  : Option[LinkedHashMap[ReqBody,(Method,Option[LinkedHashMap[Field,Int]])]]
  = None
  var _rspMap
  : Option[LinkedHashMap[RspBody,(Method,Option[LinkedHashMap[Field,Int]])]]  
  = None

  def mapMaker : Cartographer[ReqBody,RspBody]

  override def reqMap
  : Option[LinkedHashMap[ReqBody,(Method,Option[LinkedHashMap[Field,Int]])]]
  = {
    _reqMap match {
      case None => {
	_reqMap = mapMaker.stdReqMap
	_reqMap
      }
      case _ => _reqMap
    }
  }
  override def rspMap
  : Option[LinkedHashMap[RspBody,(Method,Option[LinkedHashMap[Field,Int]])]]  
  = {
    _rspMap match {
      case None => {
	_rspMap = mapMaker.stdRspMap
	_rspMap
      }
      case _ => _rspMap
    }
  }
}

trait BondHandler[
  ReqBody <: {def getClass : Class[_]},
  RspBody <: {def getClass : Class[_]}
] extends BondProxy[ReqBody,RspBody] {
  def getFVal( fld : Field ) = {
      val fAcc = fld.isAccessible
      fld.setAccessible( true )
      val rslt = fld.get( src )
      fld.setAccessible( fAcc )	
      rslt	
    }

    // BUGBUG -- LGM -- this code is duplicated
    def requestActuals(
      request : JustifiedRequest[ReqBody,RspBody],
      mthd : Method,
      fldmap : Option[LinkedHashMap[Field,Int]]
    ) : Array[Object] = {
      val flds = request.body.getClass.getDeclaredFields
      fldmap match {
	case None => {
	  for( fld <- flds ) yield {
	    getFVal( fld )
	  }
	}
	case Some( fmap ) => {
	  val ans = new Array[Object]( flds.length )      

	  for( fld <- flds ) {	    
	    for( p <- fmap.get( fld ) ) {
	      ans( p ) = getFVal( fld )
	    }
	  }

	  ans
	}
      }      
    }
    def responseActuals(
      response : JustifiedResponse[ReqBody,RspBody],
      mthd : Method,
      fldmap : Option[LinkedHashMap[Field,Int]]
    ) : Array[Object] = {
      val flds = response.body.getClass.getDeclaredFields
      fldmap match {
	case None => {
	  for( fld <- flds ) yield {
	    getFVal( fld )
	  }
	}
	case Some( fmap ) => {
	  val ans = new Array[Object]( flds.length )      

	  for( fld <- flds ) {	    
	    for( p <- fmap.get( fld ) ) {
	      ans( p ) = getFVal( fld )
	    }
	  }

	  ans
	}
      }      
    }    
}

case class Representation[
  ReqBody <: {def getClass : Class[_]},
  RspBody <: {def getClass : Class[_]}
](
  territory    : Object,
  mapMaker     : TerritorialCartographer[ReqBody,RspBody],
  dsgn8tr      : URI,
  reqs         : ListBuffer[JustifiedRequest[ReqBody,RspBody]],
  rsps         : ListBuffer[JustifiedResponse[ReqBody,RspBody]],
  env          : Option[LinkedHashMap[URI,Socialite[ReqBody,RspBody]]],
  tm           : TraceMonitor[ReqBody,RspBody]
) extends Messenger[ReqBody,RspBody](
  dsgn8tr, reqs, rsps, env, tm
  ) with BondHandler[ReqBody,RspBody] with Territory {

    override def handle( request : JustifiedRequest[ReqBody,RspBody] ) = {
      val rslt = (for ( m <- reqMap )
		  yield {
		    for ( ( mthd, optFldMap ) <- m.get( request.body ) )
		    yield {	  
		      mthd.invoke(
			src,
			requestActuals( request, mthd, optFldMap )
		      )
 		    }})
	
	rslt match {
	  case None => false
	  case _ => true
	}
    }

    override def handle( response : JustifiedResponse[ReqBody,RspBody] ) = {
      val rslt = (for ( m <- rspMap )
		  yield {
		    for ( ( mthd, optFldMap ) <- m.get( response.body ) )
		    yield {
		      mthd.invoke(
			trgt,
			responseActuals( response, mthd, optFldMap )
		      )
		    }})

	rslt match {
	case None => false
	case _ => true
      }
    }
}
