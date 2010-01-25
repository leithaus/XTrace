// -*- mode: Scala;-*- 
// Filename:    Diff.scala 
// Authors:     lgm                                                    
// Creation:    Sun Jan 24 20:08:25 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.differential

trait Differential[Name, NSeq <: NmSeq[Name]] 
extends NmSeqOps[Name,NSeq] {
  def regularNull( supp : NSeq ) : RegularNullity[Name,NSeq]
  def regularUnit( supp : NSeq ) : RegularUnity[Name,NSeq]
  def partial( x : Name, rtype : RegularType[Name, NSeq] )
  : RegularType[Name, NSeq] = {
    rtype match {
      case RegularMention( y, supp ) => {
	if ( x == y ) {
	  regularUnit( supp )
	}
	else {
	  regularNull( supp )
	}
      }
      case RegularNullity( supp ) => regularNull( supp )
      case RegularUnity( supp ) => regularNull( supp )
      case RegularSum( s, supp ) => {
	RegularSum(
	  s.map(
	    {
	      ( rt : RegularType[Name,NSeq]) => {
		partial( x, rt )
	      }
	   }
	  ),
	  supp
	)
      }
      case RegularProduct( s, supp ) => {
	val right = s.dropRight( 1 )
	RegularSum[Name,NSeq](
	  List(
	    RegularProduct[Name,NSeq](
	      List(
		partial( x, s( 0 ) ),
		RegularProduct[Name,NSeq](
		  right,
		  supp
		)
	      ),
	      supp
	    ),
	    RegularProduct[Name,NSeq](
	      List(
		s( 0 ),
		partial(
		  x,
		  RegularProduct[Name,NSeq]( right, supp )
		)
	      ),
	      supp
	    )
	  ),
	  supp
	)
      }
      case RegularFixPt( v, e, supp ) => {
	val z = fresh match {
	  case None => throw new Exception( "out of names" )
	  case Some( fn ) => fn
	}
	RegularSum[Name,NSeq](
	  List( 
	    RegularFixPt(
	      z, 
	      partial(
		x,
		RegularWeakening(
		  z,
		  RegularFPEnv( v, e, rtype, supp ),
		  supp
		)
	      ),
	      supp
	    ),
	    RegularProduct(
	      List(
		partial(
		  v,
		  RegularFPEnv(
		    v,
		    e,
		    rtype,
		    supp
		  )
		),
		RegularMention( z, supp )
	      ),
	      supp
	    )
	  ),
	  supp
	)
      }
      case RegularFPEnv( v, e, s, supp ) => {
	RegularSum(
	  List(
	    RegularFPEnv(
	      v,
	      partial( x, e ),
	      s,
	      supp
	    ),
	    // BUGBUG -- lgm -- have i got the association correct
	    RegularProduct(
	      List(
		RegularFPEnv(
		  v,
		  partial( v, e ),
		  s,
		  supp
		),
		partial( x, s )
	      ),
	      supp
	    )
	  ),
	  supp
	)
      }
      case RegularWeakening( v, e, supp ) => {
	if ( x == v ) {
	  regularNull( supp )
	}
	else {
	  RegularWeakening( v, partial( x, e ), supp )
	}
      }
    }
  }
}
