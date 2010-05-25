// -*- mode: Scala;-*- 
// Filename:    tree.scala 
// Authors:     lgm                                                    
// Creation:    Tue May 25 03:02:51 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.validation.zipper

trait Tree[A]
class TreeItem[A]( val item : A ) extends Tree[A]
object TreeItem {
  def apply[A]( item : A ) = { new TreeItem( item ) }
  def unapply[A]( tree : TreeItem[A] )
  : Option[( A )] = {
    Some( ( tree.item ) )
  }
}
class TreeSection[A](
  val section: List[Tree[A]]
) extends Tree[A]
object TreeSection {
  def apply[A]( section : List[Tree[A]] ) = { new TreeSection( section ) }
  def unapply[A]( tree : TreeSection[A] )
  : Option[( List[Tree[A]] )] = {
    Some( ( tree.section ) )
  }
}
