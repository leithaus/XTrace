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

class Cloner extends Visitor[LINQAttr] () {
  
  def clone(e: LINQAttr) : LINQAttr = {
    val w = new Walker(this, null);
    val cloned = w walk e
    cloned
  } 

  
   def visit(qe: QueryExp, rfrom: LINQAttr, rqbody: LINQAttr) = {
     QueryExp( rfrom.asInstanceOf[FromClause], rqbody.asInstanceOf[QueryBody] )
   }
   def visit(fc: FromClause, rin: LINQAttr) = {
     FromClause(fc.tp, fc.variable, rin.asInstanceOf[Exp])
   }  
   def visit(qb: QueryBody, rqbclauses: List[LINQAttr], rselGby: LINQAttr, rqcont: Option[LINQAttr])  = {
     QueryBody (rqbclauses.asInstanceOf[List[BodyClause]], rselGby.asInstanceOf[SG], rqcont.asInstanceOf[Option[QueryCont]])
   } 
   def visit(qc: QueryCont, rqbody: LINQAttr) = {
     QueryCont(qc.variable, rqbody.asInstanceOf[QueryBody])
   } 
   def visit(lc: LetClause, rrhs: LINQAttr) = {
     LetClause(lc.lhs, rrhs.asInstanceOf[Exp])
   } 
   def visit(wc: WhereClause, rbooltest: LINQAttr) = {
     WhereClause(rbooltest.asInstanceOf[Exp])
   } 
   def visit(jc: JoinClause, rinnerexp: LINQAttr, rlhs: LINQAttr, rrhs: LINQAttr) = {
     JoinClause(jc.tp, jc.innervar, rinnerexp.asInstanceOf[Exp], rlhs.asInstanceOf[Exp], rrhs.asInstanceOf[Exp])
   } 
   def visit(jic: JoinIntoClause, rjc: LINQAttr) = {
     JoinIntoClause(rjc.asInstanceOf[JoinClause], jic.result)
   } 
   def visit(obc: OrderByClause, rorderings: List[LINQAttr]) = {
     OrderByClause(rorderings.asInstanceOf[List[Ordering]])
   }  
   def visit(oc: Ordering, rord: LINQAttr) = {
     Ordering(rord.asInstanceOf[Exp], oc.direction)
   }  
   def visit(sc: SelectClause, rselexp : LINQAttr) = {
     SelectClause(rselexp.asInstanceOf[Exp])
   }   
   def visit(gbc: GroupByClause, rresexp: LINQAttr, rclusterexp: LINQAttr) = {
     GroupByClause(rresexp.asInstanceOf[Exp], rclusterexp.asInstanceOf[Exp])
   } 
   def visit(le: LogicalExp, rrands: List[LINQAttr]) = {
     le match {
       case AndExp(_) => AndExp(rrands.asInstanceOf[List[Exp]])
       case OrExp(_) => OrExp(rrands.asInstanceOf[List[Exp]])
     }
   } 
   def visit(bo: BinaryOp, rlhs: LINQAttr, rrhs: LINQAttr) = {
     bo match {
       case EqExp(_, _, oper) => EqExp(rlhs.asInstanceOf[Exp], rrhs.asInstanceOf[Exp], oper)
       case RelExp(_, _, oper) => RelExp(rlhs.asInstanceOf[Exp], rrhs.asInstanceOf[Exp], oper)
       case AddExp(_, _, oper) => AddExp(rlhs.asInstanceOf[Exp], rrhs.asInstanceOf[Exp], oper)
       case MultExp(_, _, oper) => MultExp(rlhs.asInstanceOf[Exp], rrhs.asInstanceOf[Exp], oper)
     }
   } 
   def visit(ue: UnaryExp, rexp: LINQAttr) = {
     UnaryExp(ue.oper, rexp.asInstanceOf[Exp])
   } 
   def visit(stmt: Stmt, rdotseparated: List[LINQAttr]) = {
     Stmt(rdotseparated.asInstanceOf[List[Exp]])
   } 
   def visit(ne: NestedExp, rexp: LINQAttr) = {
     NestedExp(rexp.asInstanceOf[Exp])
   } 
   def visit(mc: MethodCall, rargs: List[LINQAttr]) = {
     MethodCall(mc.methodName, mc.optioncast, rargs.asInstanceOf[List[Exp]])
   } 
   def visit(ve: ValueExp) = {
     ve
   } 
   def visit(net: NewExpTraditional, rrhss: List[LINQAttr]) = {
     NewExpTraditional(net.tp, rrhss.asInstanceOf[List[Exp]])
   } 
   def visit(newi: NewExpWithInit, rrhss: List[LINQAttr]) = {
     NewExpWithInit(newi.tp, newi.lhss, rrhss.asInstanceOf[List[Exp]])
   }

}

object Cloner extends Cloner {} 
