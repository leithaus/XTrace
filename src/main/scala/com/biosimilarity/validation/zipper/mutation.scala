// -*- mode: Scala;-*- 
// Filename:    mutation.scala 
// Authors:     lgm                                                    
// Creation:    Tue May 25 12:59:14 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.validation.zipper

trait ZipperMutation[A] {
  def update(
    location : Location[A],
    tree : Tree[A]
  ) : Location[A] = {
    location match {
      case Location( _, ctxt ) =>
	Location( tree, ctxt )
    }
  }
  def insertRight(
    location : Location[A],
    tree : Tree[A]
  ) : Location[A] = {
    location match {
      case Location( _, Top( ) ) => {
	throw new Exception( "insert of top" )
      }
      case Location(
	curr,
	TreeContext( left, up, right )
      ) => {
	Location(
	  curr,
	  TreeContext( left, up, tree :: right )
	)	
      }
    }    
  }
  def insertLeft(
    location : Location[A], tree : Tree[A]
  ) : Location[A] = {
    location match {
      case Location( _, Top( ) ) => {
	throw new Exception( "insert of top" )
      }
      case Location(
	curr,
	TreeContext( left, up, right )
      ) => {
	Location(
	  curr,
	  TreeContext( tree :: left, up, right )
	)	
      }
    }    
  }
  def insertDown(
    location : Location[A], tree : Tree[A]
  ) : Location[A] = {
    location match {
      case Location( TreeItem( _ ), _ ) => {
	throw new Exception( "down of item" )
      }
      case Location(
	TreeSection( progeny ),
	ctxt
      ) => {
	Location(
	  tree,
	  TreeContext( Nil, ctxt, progeny )
	)
      }
    }
  }
  def delete(
    location : Location[A], tree : Tree[A]
  ) : Location[A] = {
    location match {
      case Location( _, Top( ) ) => {
	throw new Exception( "delete of top" )
      }
      case Location(
	_,
	TreeContext( left, up, r :: right )
      ) => {
	Location(
	  r,
	  TreeContext( left, up, right )
	)
      }
      case Location(
	_,
	TreeContext( l :: left, up, Nil )
      ) => {
	Location(
	  l,
	  TreeContext( left, up, Nil )
	)
      }
      case Location(
	_,
	TreeContext( Nil, up, Nil )
      ) => {
	Location( TreeSection( Nil ), up )
      }
    }
  }
}
