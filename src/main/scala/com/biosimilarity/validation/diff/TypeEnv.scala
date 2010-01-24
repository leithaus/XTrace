// -*- mode: Scala;-*- 
// Filename:    TypeEnv.scala 
// Authors:     lgm                                                    
// Creation:    Fri Jan 22 10:19:59 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.differential

import scala.collection.MapProxy
import scala.collection.mutable.HashMap

trait TypeEnvironment[Name, NSeq <: NmSeq[Name]]
      extends NmSeqOps[Name,NSeq]
      with MapProxy[Name,RegularType[Name,NSeq]] {
	val emptiness = new HashMap[Name,RegularType[Name,NSeq]]()
	def types : Seq[RegularType[Name,NSeq]]	= {
	  self.values.toSeq
	}
	override def support : NSeq = {
	  self.keys.toSeq.asInstanceOf[NSeq]
	}
	override def self = emptiness	
	override def -( key : Name ) = self.-( key )
	override def +[B >: RegularType[Name,NSeq]]( 
	  kv : ( Name, B )
	) = self.+( kv )
      }

case class TypeAssignment[Name, NSeq <: NmSeq[Name]](
  map : HashMap[Name,RegularType[Name,NSeq]]
) extends TypeEnvironment[Name,NSeq] {
  override def self = map
}

case class TypeEnvRestriction[Name, NSeq <: NmSeq[Name]](
  x : Name,
  env : TypeAssignment[Name, NSeq]
) extends TypeEnvironment[Name,NSeq] {
  var _memo : Option[HashMap[Name,RegularType[Name,NSeq]]] = None
  override def self = {
    _memo match {
      case None => {
	val map = new HashMap[Name,RegularType[Name,NSeq]]()
	env.get( x ) match {
	  case Some( t ) => {
	    val np : ( Name, RegularType[Name,NSeq] ) = ( x, t )
	    map += np
	  }
	  case None => {
	  }
	}
	_memo = Some( map )
	map
      }      
      case Some( map ) => map
    }
  }
  override def types : Seq[RegularType[Name,NSeq]] = {
    this.values.toSeq
  }
}
