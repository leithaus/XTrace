// -*- mode: Scala;-*- 
// Filename:    Ops.scala 
// Authors:     lgm                                                    
// Creation:    Fri Jan 22 11:00:56 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.differential

trait NmSeq[Name] extends Seq[Name]

trait NmSeqOps[Name, NSeq <: NmSeq[Name]] {
  def support : NSeq
  def fresh : Option[Name] = None
}
