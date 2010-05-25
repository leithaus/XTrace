// -*- mode: Scala;-*- 
// Filename:    location.scala 
// Authors:     lgm                                                    
// Creation:    Tue May 25 03:04:44 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.validation.zipper

class Location[A](
  val tree : Tree[A],
  val ctxt : Context[A]
)
object Location {
  def apply[A]( tree : Tree[A], ctxt : Context[A] ) = {
    new Location( tree, ctxt )
  }
  def unapply[A]( loc : Location[A] )
  : Option[( Tree[A], Context[A] )] = {
    Some( ( loc.tree, loc.ctxt ) )
  }
}
