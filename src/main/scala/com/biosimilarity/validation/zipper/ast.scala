// -*- mode: Scala;-*- 
// Filename:    ast.scala 
// Authors:     lgm                                                    
// Creation:    Tue May 25 04:19:13 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.validation.zipper

case class Token[A](
  override item : A
) extends TreeItem[A]( item )
case class AST[A](
  override section : List[Tree[A]]
) extends TreeSection[A]( section )


