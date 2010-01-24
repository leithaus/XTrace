// -*- mode: Scala;-*- 
// Filename:    Regular.scala 
// Authors:     lgm                                                    
// Creation:    Fri Jan 22 08:54:44 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.differential

trait RegularType[Name, NSeq <: NmSeq[Name]]
extends NmSeqOps[Name,NSeq]

case class RegularMention[Name, NSeq <: NmSeq[Name]](
  x : Name,
  support : NSeq
) extends RegularType[Name, NSeq]

case class RegularNullity[Name, NSeq <: NmSeq[Name]](
  support : NSeq
) extends RegularType[Name, NSeq]
case class RegularUnity[Name, NSeq <: NmSeq[Name]](
  support : NSeq
) extends RegularType[Name, NSeq]

case class RegularProduct[Name, NSeq <: NmSeq[Name]](
  s : Seq[RegularType[Name,NSeq]],
  support : NSeq
) extends RegularType[Name, NSeq]

case class RegularSum[Name, NSeq <: NmSeq[Name]](
  s : Seq[RegularType[Name,NSeq]],
  support : NSeq
) extends RegularType[Name, NSeq]

case class RegularFixPt[Name, NSeq <: NmSeq[Name]](
  v : Name,
  e : RegularType[Name, NSeq],
  support : NSeq
) extends RegularType[Name, NSeq]

case class RegularFPEnv[Name, NSeq <: NmSeq[Name]](
  v : Name,
  e : RegularType[Name, NSeq],
  s : RegularType[Name, NSeq],
  support : NSeq
)

case class RegularWeakening[Name, NSeq <: NmSeq[Name]](
  v : Name,
  e : RegularType[Name, NSeq],
  support : NSeq
) extends RegularType[Name, NSeq]

case class RegularRestriction[Name, NSeq <: NmSeq[Name]](
  v : Name,
  e : RegularType[Name, NSeq]
) extends RegularType[Name, NSeq] {
  def support : NSeq = {
    val eSupp = e.support
    eSupp.findIndexOf( v.equals( _ ) ) match {
      case (-1) => {
	eSupp
      }
      case i => {
	eSupp.dropRight( eSupp.length - i ).asInstanceOf[NSeq]
      }
    }
  }
}

case class RegularProjection[Name, NSeq <: NmSeq[Name]](
  x : Name,
  typeEnv : TypeEnvironment[Name,NSeq]
) extends RegularType[Name, NSeq] {
  def project : Option[RegularType[Name, NSeq]] = {
    typeEnv.get( x )
  }
  override def support : NSeq = {
    project match {
      case Some( t ) => t.support
      // BUGBUG -- LGM -- This implementation is erroneous!
      case None => List( ).asInstanceOf[NSeq]
    }
  }
}
