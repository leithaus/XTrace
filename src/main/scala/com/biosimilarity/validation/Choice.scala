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

import scala.util.continuations._ 
//import scala.util.continuations.ControlContext._ 
import scala.collection.mutable._
import scala.actors._
import Actor._

trait WorkStatus
case class InProgress( ) extends WorkStatus
case class Paused( ) extends WorkStatus
case class Resumed( ) extends WorkStatus
case class Complete( ) extends WorkStatus
case class Aborted( ) extends WorkStatus
case class Catastrophe( msg : String )
     extends Exception( msg ) with WorkStatus

trait WorkTransition
case class Beginning( ) extends WorkTransition
case class Continuing( ) extends WorkTransition
case class Pausing( ) extends WorkTransition
case class Resuming( ) extends WorkTransition
case class Completing( ) extends WorkTransition
case class Aborting( ) extends WorkTransition

trait WorkRelatedMsg
case class BeginWork( k : Unit => Unit ) extends WorkRelatedMsg
case class DoWork( k : Unit => Unit ) extends WorkRelatedMsg
case class PauseWork( k : Unit => Unit ) extends WorkRelatedMsg
case class ResumeWork( k : Unit => Unit ) extends WorkRelatedMsg
case class StopWork( k : Unit => Unit ) extends WorkRelatedMsg

trait WorkLog[Task] {  
  def monitor : WorkMonitor[Task]
  def logStatus( worker : Handler[Task] ) : Unit = {
    monitor.traceEvent(
	this,
	(
	  "<" + "worker" + ">"
	  + "<" + "task" + ">"
	  + worker.task
	  + "</" + "task" + ">"
	  + "<" + "status" + ">"
	  + (worker.status match {
		case Some( s ) => s
		case None => "none" })
	  + "</" + "status" + ">"
	  + "</" + "worker" + ">"
	)
      )
  }

  def logTransition(
    worker     : Handler[Task],
    transition : WorkTransition
  ) : Unit = {
    monitor.traceEvent(
	this,
	(
	  "<" + "worker" + ">"
	  + "<" + "task" + ">"
	  + worker.task
	  + "</" + "task" + ">"
	  + "<" + "transition" + ">"
	  + transition
	  + "</" + "transition" + ">"
	  + "</" + "worker" + ">"
	)
      )
  }

  def logError( msg : String ) : Unit = {
    monitor.traceEvent(
	this,
	(
	  "<" + "error " + ">"
	  + msg
	  + "</" + "error" + ">"
	)
      )
  }
  
  def showWorkerStatus( workers : List[Handler[Task]] ) : Unit = {
    monitor.traceEvent(
      this,
	(
	  "<" + "snapshot" + ">"
	)
      )
    for( w <- workers ) {
      logStatus( w )
    }
    monitor.traceEvent(
      this,
	(
	  "</" + "snapshot" + ">"
	)
      )
  }
}

trait WorkManager[Task] {
  type Mgr[_] <: WorkManager[_]
  type Wkr <: Worker[Task,Mgr]

  def workers : Sequence[Wkr]
  //def winner : Option[Wkr]  

  //def winner( ftw : Wkr ) : Unit  
  def manage( task : Task ) : Wkr

  def manage( tasks : Sequence[Task] ) : Sequence[Wkr] = {
    for ( task <- tasks )
    yield {      
      val worker = manage( task );
      worker.start;
      worker
    }
  }
  def workIt( workforce : Sequence[Wkr] )( k : Unit => Unit ) : Unit = {
    for( w <- workforce ) {
      w ! BeginWork( k )
    }
  }
}

trait Handler[Task] {
  def task : Task
  def status : Option[WorkStatus]
  def status( ws : WorkStatus ) : Unit
  def success : Option[Boolean]
  def success( s : Boolean ) : Unit
  def mgr : WorkManager[Task]
  def handle( task : Task, k : Unit => Unit ) : Unit
}

abstract class Worker[Task, WMgr[_]](
  task : Task,
  mgr  : WorkManager[Task] with WorkLog[Task] with WMgr[Task]
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
  def logStateTransitionError( msg : WorkRelatedMsg ) : Unit = {
    mgr.logError(
      (
	"invalid transition request : "
	+ " in state " + status
	+ " requesting " + msg
      )
    )
  }

  override def act() =
    {
      react
      {
	// Simple worker state machine
	case BeginWork( k ) => {
	  status match {
	    case None => {
	      status( InProgress() )
	      mgr.logTransition( this, Beginning( ) )
	      handle( task, k )
	    }
	    case _ => {
	      logStateTransitionError( BeginWork( k ) )
	    }
	  }
	  act()
	}
	case DoWork( k ) => {
	  status match {
	    case Some( InProgress( ) ) => {
	      status( InProgress() )
	      mgr.logTransition( this, Continuing( ) )
	      Thread.sleep( 100 )
	      handle( task, k )
	    }
	    case _ => {
	      logStateTransitionError( DoWork( k ) )
	    }
	  }
	  act()
	}
	case PauseWork( k ) => {
	  status match {
	    case Some( InProgress( ) ) => {
	      mgr.logTransition( this, Pausing( ) )
	      status( Paused() )
	    }
	    case _ => {
	      logStateTransitionError( PauseWork( k ) )
	    }
	  }
	  act()
	}
	case ResumeWork( k ) => {
	  status match {
	    case Some( Paused( ) ) => {
	      status( Resumed() )
	      mgr.logTransition( this, Resuming( ) )	  
	      handle( task, k )
	    }
	    case _ => {
	      logStateTransitionError( ResumeWork( k ) )
	    }
	  }
	  act()
	}
	case StopWork( k ) => {
	  status match {
	    case None => {
	      logStateTransitionError( StopWork( k ) )
	    }
	    case Some( Aborted( ) ) => {
	      logStateTransitionError( StopWork( k ) )
	    }
	    case Some( Complete( ) ) => {
	      logStateTransitionError( StopWork( k ) )
	    }
	    case Some( _ ) => {
	      status( Aborted() )
	      mgr.logTransition( this, Aborting( ) )	      
	      k( )
	    }
	  }
	}
      }
    }
}

abstract class Choice[Task](
  workers : ListBuffer[Worker[Task,Choice]]
) extends WorkManager[Task] with WorkLog[Task]
{
  class ChoiceState {
    var ftw : Option[Wkr] = None
    var counters : Option[Int] = None

    def winner() = ftw
    def winner( wtf : Wkr ) = {
      ftw = Some( wtf )
    }
    def showTrackCount() : Unit = {
      for( cntrs <- counters ) {
	println( "cntrs : " + cntrs )
      }
    }
    def track( s : Int ) : Unit = {
      counters = Some( s )
    }
    def untrack() : Unit = {
      counters match {
	case Some( s ) => { counters = Some( s - 1 ); }
	case None => { }
      }
    }
    def tracking() : Option[Boolean] = {
      for( cexes <- counters ) yield { cexes != 0 }
    }
  }

  val cstate : ChoiceState = new ChoiceState()
  def winner() = cstate.winner()
  def winner( wtf : Wkr ) = cstate.winner( wtf )
  
  def select( tasks : Sequence[Task] ) : Option[Task] = {
    reset{      
      monitor.openMonitoringSession( this )
      val workforce = manage( tasks )
      cstate.track( workforce.length - 1 )

      shift {
	( k : Unit => Unit ) =>
	  {
	    workIt( workforce )( k ) ;
	  }
      }

      winner() match {
	case None => {
	  println( "No winner" )
	  logError( "No winner" )
	}
	case Some( v ) => {
	  println( "A winner" )
	  // Note: we cannot close the monitoring session until all
	  // the StopWork processing has completed. So, we grab a
	  // continuation and ensure that we return to the point
	  // before the session was closed. 
	  // Question: does this only work because
	  // closeMonitoringSession is idempotent?
	  logStatus( v )
	  reset {
	    shift {
	      ( ks : Unit => Unit ) => {
		for( w <- workforce if w != v ) {
		  w ! StopWork( ks )
		};
	      }
	    }
	    cstate.untrack()
	    for( trax <- cstate.tracking() if !trax ) {
	      monitor.closeMonitoringSession( this )
	    }
	    ()
	  }
	}
      };          
    }
    println( "winner : " + winner() )
    for ( w <- winner() ) yield { w.task }
  }
}

// A comprehension

abstract class Comprehension[A](  
  val condition : A => Boolean,
  val width : Int
) extends WorkManager[A] with WorkLog[A]
{
  var ftw : Option[ListBuffer[Wkr]] = None
  var ctw : Option[ListBuffer[Wkr]] = None
  var utw : Option[ListBuffer[Wkr]] = None
  var cursor : Int = 0

  val workforce : ListBuffer[Wkr] = new ListBuffer[Wkr]()

  def ~~( ltsk : A, rtsk : A ) : Boolean

  def witnesses = ftw
  def witnesses( wtf : ListBuffer[Wkr] ) = {
    ftw = Some( wtf )
  }
  def counterExamples = ctw
  def counterExamples( wtf : ListBuffer[Wkr] ) = {
    ctw = Some( wtf )
  }
  def undecided = utw
  def undecided( wtf : ListBuffer[Wkr] ) = {
    utw = Some( wtf )
  }
  
  def full() : Boolean = {
    cursor >= width
  }

  def testify( wtf : Wkr ) : Unit = {
    val worker = wtf.asInstanceOf[PredicateWorker[A]]
    worker match {
      case Predicated( task, mgr, truth ) => {	
	undecided match {
	  case None => {
	    val wnrs = new ListBuffer[Wkr]()
	    wnrs += wtf
	    undecided( wnrs )
	  }
	  case Some( wnrs ) => {
	    wnrs += wtf
	  }
	}
      }
      case Witness( task, mgr, truth ) => {
	witnesses match {
	  case None => {
	    val wnrs = new ListBuffer[Wkr]()
	    wnrs += wtf
	    witnesses( wnrs )
	  }
	  case Some( wnrs ) => {
	    wnrs += wtf
	  }
	}
      }
      case CounterExample( task, mgr, truth ) => {
	counterExamples match {
	  case None => {
	    val wnrs = new ListBuffer[Wkr]()
	    wnrs += wtf
	    counterExamples( wnrs )
	  }
	  case Some( wnrs ) => {
	    wnrs += wtf
	  }
	}
      }
    }

    cursor += 1;
  }

  def untested( task : A ) : Boolean = {    
    (
      (counterExamples match {
	case None => true
	case Some( cexes ) => {
	  ( true /: cexes )( { ( acc : Boolean, w : Wkr ) => {
	    acc && ~~( task, w.task )
	  }
				      } )
	}
      })
      &&
      (witnesses match {
	case None => true
	case Some( wits ) => {
	  ( true /: wits )( { ( acc : Boolean, w : Wkr ) => {
	    acc && ~~( task, w.task )
	  }
				} )
	}
      })
    )
  }

  override def workers = {
    workforce
  }
  def manage( task : A ) : Wkr = {
    Predicated( task, this, true ).asInstanceOf[Wkr]
  }
  override def manage( tasks : Sequence[A] ) : Sequence[Wkr] = {
    for ( i <- 1 to width; task = tasks( i ) if untested( task ) )
    yield {      
      val worker = manage( task );
      workforce += worker;
      worker.start;
      worker
    }
  }
  override def workIt( wrkfrc : Sequence[Wkr] )( k : Unit => Unit )
  : Unit = {
    for( i <- 1 to width; w = wrkfrc( i ) ) {
      w ! BeginWork( k )
    }
  }

  def select( tasks : Sequence[A] ) : Option[Sequence[A]] = {
    reset{
      monitor.openMonitoringSession( this )
      shift {
	( k : Unit => Unit ) =>
	  {
	    workIt( manage( tasks ) )( k ) ;
	  }
      }
      witnesses match {
	case None => {
	}
	case Some( v ) => {
	  reset {
	    shift {
	      ( ks : Unit => Unit ) => {
		for( w <- workforce if w != v ) { w ! StopWork( ks ) };
	      }
	    }
	    monitor.closeMonitoringSession( this );
	    ()
	  }
	}
      };
      ()
    }
    for( wits <- witnesses ) yield {
      for( w <- wits ) yield { w.task }
    }
  }
}

abstract class PredicateWorker[A](
  task : A, mgr : Comprehension[A]
) extends Worker[A,Comprehension]( task, mgr ){
  def stateless : Boolean
  override def handle( task : A, k : Unit => Unit ) = {
    if ( mgr.condition( task ) ) {
      status( Complete() )
      mgr.logTransition( this, Completing( ) )	  
      mgr.testify( Witness( task, mgr, stateless ).asInstanceOf[mgr.Wkr] )
      if ( mgr.full() ) k()
    }
    else {
      if (!stateless) {
	this ! DoWork( k )
      }
      else {
	status( Complete() )
	mgr.logTransition( this, Completing( ) )	  
	mgr.testify(
	  CounterExample( task, mgr, stateless ).asInstanceOf[mgr.Wkr]
	)
	if ( mgr.full() ) k()
      }
    }
  }
}

case class Predicated[A](
  task : A, mgr : Comprehension[A], stateless : Boolean
) extends PredicateWorker[A]( task, mgr )

case class Witness[A](
  task : A, mgr : Comprehension[A], stateless : Boolean
) extends PredicateWorker[A]( task, mgr )

case class CounterExample[A](
  task : A, mgr : Comprehension[A], stateless : Boolean
) extends PredicateWorker[A]( task, mgr )

// Unit test
case class ModWorker(
  task : Int, mgr : Choice[Int]
) extends Worker[Int,Choice]( task, mgr ) {
  override def handle( task : Int, k : Unit => Unit ) = {
    if ((task % 2) == 0) {
      status( Complete() )
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
  val monitor : WorkMonitor[Int] = new WorkMonitor[Int]()
  def manage( task : Int ) : Wkr = {
    ModWorker( task, this ).asInstanceOf[Wkr]
  }
}

case object TheModChoice
     extends ModChoice( new ListBuffer[Worker[Int,Choice]]() ) {              
}
