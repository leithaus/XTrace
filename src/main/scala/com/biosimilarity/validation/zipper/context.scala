// -*- mode: Scala;-*- 
// Filename:    context.scala 
// Authors:     lgm                                                    
// Creation:    Tue May 25 03:04:00 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.validation.zipper

trait Context[A]
case class Top[A]( ) extends Context[A]
class TreeContext[A](
  val left : List[Tree[A]],
  val ctxt : Context[A],
  val right : List[Tree[A]]
) extends Context[A]
object TreeContext {
  def apply[A](
    left : List[Tree[A]],
    ctxt : Context[A],
    right : List[Tree[A]] ) = {
    new TreeContext( left, ctxt, right )
  }
  def unapply[A]( ctxt : TreeContext[A] )
  : Option[( List[Tree[A]], Context[A], List[Tree[A]] )] = {
    Some( ( ctxt.left, ctxt.ctxt, ctxt.right ) )
  }
}
