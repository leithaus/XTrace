// -*- mode: Scala;-*- 
// Filename:    Ordering.scala 
// Authors:     lgm                                                    
// Creation:    Sun Jan 24 09:31:26 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.differential

trait SubtermRelation[Name, NSeq <: NmSeq[Name]] {
  self : RegularTypeCheck[Name, NSeq] with NmSeqOps[Name, NSeq] =>
    def <(
      n : Name,
      rtype : RegularType[Name, NSeq],
      rtermL : RegularTerm[Name, NSeq],
      rtermR : RegularTerm[Name, NSeq]
    ) : Boolean = {
      rtype match {
	case RegularMention( x, supp ) => {
	  if ( x == n ) {
	    inhabits(
	      emptyTypeEnv,
	      rtermL,
	      rtype
	    )
	  }
	  else {
	    false
	  }
	}
	case RegularSum( ts, supp ) => {
	  rtermR match {
	    case RegularInjection( tag, u ) => {
	      val rInj = rtermR.asInstanceOf[RegularInjection[Name,NSeq]]
	      <( n, ts( rInj.inject ), rtermL, u )
	    }
	  }
	}
	case RegularProduct( ts, supp ) => {
	  rtermR match {
	    case RegularTuple( rxt ) => {
	      if ( ts.length > rxt.length ) {
		val xt =
		  ( ( false, 0 ) /: rxt )( 
		    {
		      (
			acc : ( Boolean, Int ),
			e : RegularTerm[Name, NSeq]
		      ) => {
			acc match {
			  case ( rslt, pos ) => {
			    (
			      rslt || <( n, ts( pos ), rtermL, rxt( pos )),
			      pos + 1
			    )
			  }
			}	      
		      }
		  }
		)
		if (! xt._1 ) {
		  val offset = ts.length - rxt.length
		  val yt =
		    ( ( false, 0 ) /: rxt )( 
		      {
			(
			  acc : ( Boolean, Int ),
			  e : RegularTerm[Name, NSeq]
			) => {
			  acc match {
			    case ( rslt, pos ) => {
			      (
				rslt || <( n, ts( pos + offset ), rtermL, rxt( pos )),
				pos + 1
			      )
			    }
			  }	      
			}
		      }
		    ) 
		  yt._1
		}
		else {
		  true
		}
	      }
	      else {
		false
	      }
	    }
	  }
	}
	case RegularFixPt( v, e, supp ) => {
	  rtermR match {
	    case RegularContainer( t ) => {
	      <( n, RegularFPEnv( v, e, rtype, supp ), rtermL, t )
	    }
	    case _ => false
	  }
	}
	case RegularFPEnv( v, e, s, supp ) => {
	  if (! <( n, e, rtermL, rtermR ) ) {
	    rtermR match {
	      case RegularContainer( t ) => {
		<( n, s, rtermL, t ) && <( n, e, t, rtermR )
	      }
	      case _ => false
	    }
	  }
	  else {
	    false
	  }
	}
	case RegularWeakening( v, e, supp ) => {
	  <( n, e, rtermL, rtermR )
	}
      }
    }
}
