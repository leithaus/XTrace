// -*- mode: Scala;-*- 
// Filename:    Term.scala 
// Authors:     lgm                                                    
// Creation:    Sun Jan 24 09:38:19 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.differential

trait RegularTerm[Name, NSeq <: NmSeq[Name]] {
}

case class RegularInjection[Name, NSeq <: NmSeq[Name]](
  tag : Seq[Boolean],
  s : RegularTerm[Name, NSeq]
) extends RegularTerm[Name, NSeq] {
  def inject : Int = {
    val xt =
      ( ( 0, 0 ) /: tag )( 
	{ 
	  ( acc : ( Int, Int ), e : Boolean ) => {
	    acc match {
	      case ( rslt, pos ) => {
		(
		  (
		    rslt
		    +
		    (
		      if ( e ) {
			Math.pow( 2, pos ).asInstanceOf[Int]
		      }
			else { 0 }
		    )
		  ),
		  pos + 1
		)
	      }
	    }
	  }
	}
      )
    xt._1
  }
}

case class RegularTuple[Name, NSeq <: NmSeq[Name]](
  s : Seq[RegularTerm[Name, NSeq]]
) extends RegularTerm[Name, NSeq]

case class RegularCon[Name, NSeq <: NmSeq[Name]](
  s : RegularTerm[Name, NSeq]
) extends RegularTerm[Name, NSeq]
