/******************************************************************************* 
 * This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 *     Miguel Garcia, http://www.sts.tu-harburg.de/people/mi.garcia 
 *******************************************************************************/ 


package sts.linq

import AbstractSyntax._

object T1 extends Cloner {
  var fired = false; 
   override def visit(qe: QueryExp, rfrom: LINQAttr, rqbody: LINQAttr) = {
     qe.qbody.qcont match {
       case None => super.visit(qe, rfrom, rqbody)
       case Some(qcontinuation) => {
         val newIn = QueryExp(qe.from, QueryBody(qe.qbody.qbclauses, qe.qbody.selGby, None))
         val newFrom = FromClause(List(), qcontinuation.variable, newIn)
         val newBody = qcontinuation.qbody
         fired = true; 
         QueryExp(newFrom, newBody) 
       }
     }
   }
}

sealed case class LinearQuery(clauses: List[BodyClause], selGby: SG) {
  import scala.collection.mutable
  private val declaredVars : List[Option[String]] = 
    clauses map {
      case FromClause(_, variable, _) => Some(variable)
      case LetClause(lhs, _) => Some(lhs)
      case WhereClause(_) => None
      case OrderByClause(_) => None
      case JoinClause(_, innervar, _, _, _) => Some(innervar)
      case JoinIntoClause(_, result) => Some(result)
    }
  val visibleVars = mutable.Map.empty[BodyClause, List[String]]  
  private var inScope : List[String] = List()
  (declaredVars zip clauses) foreach { t =>
    val optVar = t._1
    val currentClause = t._2
    if (!optVar.isEmpty) 
      inScope = inScope ::: List(optVar.get)
    visibleVars.update(currentClause, inScope)
  }
  override def toString() = {
    val res = new StringBuffer();
    clauses foreach { c => res.append(visibleVars(c).mkString(",") + "\t\t" + Comprehend(c) + "\n") }
    res.append(Comprehend(selGby))
    res.toString
  }
}

object Linearize {
   def apply(qe: QueryExp) : LinearQuery = {
     LinearQuery(qe.from :: qe.qbody.qbclauses, qe.qbody.selGby)
   }
}


object Transformer {
  def inlinecontinuations(e: QueryExp) : QueryExp = {
    val w = new Walker(T1, null);
    var output = e; 
    do { 
      T1.fired = false;
      output = (w walk output).asInstanceOf[QueryExp]   
    } while (T1.fired)
    output
  }
  def comprehend(e: QueryExp) : String = Comprehend(e)
}

