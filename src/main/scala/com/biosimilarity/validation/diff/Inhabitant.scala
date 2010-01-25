// -*- mode: Scala;-*- 
// Filename:    Inhabitant.scala 
// Authors:     lgm                                                    
// Creation:    Sun Jan 24 09:33:46 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.differential

trait RegularTypeCheck[Name, NSeq <: NmSeq[Name]] {
  def inhabits(
    env : TypeEnvironment[Name, NSeq],
    rterm : RegularTerm[Name, NSeq],
    rtype : RegularType[Name, NSeq]
  ) : Boolean = {
    rterm match {
      case RegularInjection( tag, s ) => {
	rtype match {
	  case RegularSum( ts, supp ) => {
	    inhabits(
	      env,
	      s,
	      ts( rterm.asInstanceOf[RegularInjection[Name,NSeq]].inject )
	    )
	  }
	  case RegularFPEnv( v, e, s, supp ) => {
	    val nEnv = (env.+( ( v, s ) )).asInstanceOf[TypeEnvironment[Name,NSeq]]
	    inhabits( nEnv, rterm, e )
	  }
	  case RegularWeakening( v, e, supp ) => {
	    val nEnv = (env.-( v )).asInstanceOf[TypeEnvironment[Name,NSeq]]
	    inhabits( nEnv, rterm, e )
	  }
	  case _ => false
	}
      }
      case RegularTuple( s ) => {
	rtype match {
	  case RegularProduct( ts, supp ) => {
	    if ( s.length == ts.length ) {
	      ( true /: s.zip( ts ) )(
		{
		  (
		    acc : Boolean,
		    e : ( RegularTerm[Name,NSeq], RegularType[Name,NSeq] )
		  ) => {
		    e match {
		      case ( rtx, rtt ) => {
			acc && inhabits( env, rtx, rtt )
		      }
		    }
		  }
		}
	      )
	    }
	    else {
	      false
	    }
	  }
	  case RegularFPEnv( v, e, s, supp ) => {
	    val nEnv =
	      (env.+( ( v, s ) )).asInstanceOf[TypeEnvironment[Name,NSeq]]
	    inhabits( nEnv, rterm, e )
	  }
	  case RegularWeakening( v, e, supp ) => {
	    val nEnv =
	      ( env.-( v ) ).asInstanceOf[TypeEnvironment[Name,NSeq]]
	    inhabits( nEnv, rterm, e )
	  }
	  case _ => false
	}
      }
      case RegularCon( s ) => {
	rtype match {
	  case RegularFixPt( v, e, supp ) => {
	    inhabits( env, rterm, RegularFPEnv( v, e, rtype, supp ) )
	  }
	  case RegularFPEnv( v, e, s, supp ) => {
	    val nEnv =
	      (env.+( ( v, s ) )).asInstanceOf[TypeEnvironment[Name,NSeq]]
	    inhabits( nEnv, rterm, e )
	  }
	  case RegularWeakening( v, e, supp ) => {
	    val nEnv =
	      ( env.-( v ) ).asInstanceOf[TypeEnvironment[Name,NSeq]]
	    inhabits( nEnv, rterm, e )
	  }
	  case _ => false
	}
      }
    }    
  }
}
