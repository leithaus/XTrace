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

object Exercise extends ZipperNavigation[String] {
  val arithmeticExpr1 =
    AST[String](
      List(
	AST[String](
          List(
            Token[String]( "a" ),
            Token[String]( "*" ),
            Token[String]( "b" )
          )
	),
	Token[String]( "+" ),
	AST[String](
          List(
            Token[String]( "c" ),
            Token[String]( "*" ),
            Token[String]( "d" )
          )
	)
      )
    )
  val locationOf2ndMult =
    Location[String](
      Token[String]( "*" ),
      TreeContext[String](
	List( Token[String]( "c" ) ),
	TreeContext[String](
          List(
            Token[String]( "+" ),
            AST[String](
              List(
		Token[String]( "a" ),
		Token[String]( "*" ),
		Token[String]( "b" )
              )
            )
          ),
          Top( ),
          List( )
        ),
	List( Token[String]( "d" ) )
      )
    )
  def show( depth : Int )( tree : Tree[String] ) : Unit = {
    tree match {
      case TreeItem( item : String ) => {
	val indent =
	  ( "" /: (1 to depth) )( { ( acc, d ) => acc + " " } )
	println( indent + "Leaf : " + item )
      }
      case TreeSection( section : List[Tree[String]] ) => {
	for( t <- section ){ show( depth + 2 )( t ) }
      }
    }
  }
  def pushUps( location : Location[String] ) = {
    location match {
      case Location( _, Top( ) ) => {
	println( "reached the limit" )
      }
      case Location( tree, ctxt ) => {
	show( 0 )( tree )
	up( location )
      }
    }
  }
  def doPushUps = {
    pushUps( locationOf2ndMult )
  }
}
