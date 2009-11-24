// -*- mode: Scala;-*- 
// Filename:    Choice.scala 
// Authors:     lgm                                                    
// Creation:    Sun Nov 22 13:23:24 2009 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.validation

import java.net.URI
import com.eaio.uuid.UUID

import scala.continuations._ 
import scala.continuations.ControlContext._ 
import scala.collection.mutable._
import scala.actors._
import Actor._

trait WorkStatus
case class InProgress( ) extends WorkStatus
case class Paused( ) extends WorkStatus
case class Resumed( ) extends WorkStatus
case class Complete( ) extends WorkStatus
case class Aborted( ) extends WorkStatus

trait WorkRelatedMsg
case class BeginWork( k : Unit => Unit ) extends WorkRelatedMsg
case class DoWork( k : Unit => Unit ) extends WorkRelatedMsg
case class PauseWork( k : Unit => Unit ) extends WorkRelatedMsg
case class ResumeWork( k : Unit => Unit ) extends WorkRelatedMsg
case class StopWork( ) extends WorkRelatedMsg

trait Handler[Task] {
  def task : Task
  def status : Option[WorkStatus]
  def status( ws : WorkStatus ) : Unit
  def success : Option[Boolean]
  def success( s : Boolean ) : Unit
  def handle( task : Task, k : Unit => Unit ) : Unit
}

abstract class Worker[Task, WorkManager[_]](
  task : Task,
  mgr  : WorkManager[Task]
) extends Actor with Handler[Task]
{    
  var ws : Option[WorkStatus] = None
  var s  : Option[Boolean]    = None

  // Job status
  def status = ws
  def status( wstat : WorkStatus ) = {
    ws = Some( wstat )
  }

  // High order bit
  def success : Option[Boolean] = None
  def success( b : Boolean ) = {
    s = Some( b )
  }

  override def act() =
    {
      react
      {
	case BeginWork( k ) => {
	  println( "a new beginning..." )
	  status( InProgress() )
	  handle( task, k )
	  act()
	}
	case DoWork( k ) => {
	  println( "working..." )
	  Thread.sleep( 100 )
	  status( InProgress() )
	  handle( task, k )
	  act()
	}
	case PauseWork( k ) => {
	  println( "pausing..." )
	  status( Paused() )
	  act()
	}
	case ResumeWork( k ) => {
	  println( "resuming..." )
	  status( Resumed() )
	  handle( task, k )
	  act()
	}
	case StopWork( ) => {
	  status( Aborted() )
	  println( "stopping!" )
	}
      }
    }
}

trait WorkManager[Task] {
  type Mgr[_] <: WorkManager[_]
  type Wkr <: Worker[Task,Mgr]
  def workers : ListBuffer[Wkr]
  def winner : Option[Wkr]
  def winner( ftw : Wkr ) : Unit
  def manage( task : Task ) : Wkr
  // def manage( tasks : Stream[Task] ) : Sequence[Wkr] = {
//     manage( tasks.force )
//   }
//   def manage( tasks : List[Task] ) : Sequence[Wkr] = {
  def manage( tasks : List[Task] ) : Sequence[Wkr] = {
    for ( task <- tasks )
    yield {      
      val worker = manage( task );
      // println( "Wrapping " + task + " in " + worker )
      workers += worker;
      worker.start;
      worker
    }
  }
  def workIt( k : Unit => Unit ) : Unit = {
    for( w <- workers ) {
      // println( "Here we go..." )
      w ! BeginWork( k )
    }
  }
  def showWorkerStatus : Unit = {
    for( w <- workers ) {
      println( w + " has status " + w.status )
    }
  }
}

abstract class Choice[Task](
  workers : ListBuffer[Worker[Task,Choice]]
) extends WorkManager[Task]
{
  var ftw : Option[Wkr] = None
  override def winner = ftw
  override def winner( wtf : Wkr ) = {
    ftw = Some( wtf )
  }
  def select( tasks : Stream[Task] ) : Unit = {
    select( tasks.force )
  }
  def select( tasks : List[Task] ) : Unit = {
    reset{
      shift {
	( k : Unit => Unit ) =>
	  {
	    manage( tasks );
	    workIt( k ) ;
	  }
      }
      winner match {
	case None => {
	  println( "really shouldn't get here!" )
	}
	case Some( v ) => {
	  println( "the winner is: " + v )
	  for( w <- workers if w != v ) { w ! StopWork() }
	}
      };
    }
  }
}

// A comprehension

trait Cursor {
  def size : Int
}

abstract class WorkCursor[Task]( size : Int )
	 extends Cursor with WorkManager[Task] {
	 }

abstract class Comprehension[Task](
  workers : ListBuffer[Worker[Task,Comprehension]]
) extends WorkManager[Task]
{
  var ftw : Option[ListBuffer[Wkr]] = None
  def winners = ftw
  def winners( wtf : ListBuffer[Wkr] ) = {
    ftw = Some( wtf )
  }
  def win( wtf : Wkr ) = {
    winners match {
      case None => {
	val wnrs = new ListBuffer[Wkr]()
	wnrs += wtf
	winners( wnrs )
      }
      case Some( wnrs ) => {
	wnrs += wtf
      }
    }
  }
  def select( tasks : Stream[Task] ) : Unit = {
    select( tasks.force )
  }
  def select( tasks : List[Task] ) : Unit = {
    reset{
      shift {
	( k : Unit => Unit ) =>
	  {
	    manage( tasks );
	    workIt( k ) ;
	  }
      }
      winners match {
	case None => {
	  println( "condition was empty" )
	}
	case Some( v ) => {
	  println( "the winners are: " + v )
	  for( w <- workers if w != v ) { w ! StopWork() }
	}
      };
    }
  }
}

abstract class PredicateWorker[A](
  task : (A => Boolean, A), mgr : Comprehension[(A => Boolean, A)]
) extends Worker[(A => Boolean, A),Comprehension]( task, mgr ){
  def stateless : Boolean
  override def handle( task : (A => Boolean, A), k : Unit => Unit ) = {
    if ((task._1)(task._2)) {
      status( Complete() )
      println( "found a result: " + task )
      mgr.winner( this.asInstanceOf[mgr.Wkr] )
      k()
    }
    else {
      if (!stateless) {
	this ! DoWork( k )
      }
      else {
	status( Complete() )
      }
    }
  }
}

// Unit test
case class ModWorker(
  task : Int, mgr : Choice[Int]
) extends Worker[Int,Choice]( task, mgr ) {
  override def handle( task : Int, k : Unit => Unit ) = {
    if ((task % 2) == 0) {
      status( Complete() )
      println( "found a result: " + task )
      mgr.winner( this.asInstanceOf[mgr.Wkr] )
      k()
    }
    else {
      this ! DoWork( k )
    }
  }  
}

case class ModChoice(
  cworkers : ListBuffer[Worker[Int,Choice]]
) extends Choice[Int]( cworkers ) {
  //type Wkr = Worker[Int,Choice]
  override def workers = cworkers.asInstanceOf[ListBuffer[Wkr]]
  def manage( task : Int ) : Wkr = {
    ModWorker( task, this ).asInstanceOf[Wkr]
  }
}

case object TheModChoice extends ModChoice( new ListBuffer[Worker[Int,Choice]]() )
