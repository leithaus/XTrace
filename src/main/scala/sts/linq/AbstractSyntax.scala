/*
 * This file is part of LINQExpand4Scala.
 *
 * Copyright (C) 2009 Miguel Garcia, Tech Univ Hamburg-Harburg.
 *
 * Kiama is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 *
 * Kiama is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with LINQExpand4Scala.  (See files COPYING and COPYING.LESSER.)  If not, see
 * <http://www.gnu.org/licenses/>.
 */                         

/**
 * TODO URL of ICOODB 2009 conference paper
 */

package sts.linq

/**
 * LINQ abstract syntax
 */
object AbstractSyntax {
  
    //import kiama.attribution.Attribution._
    //import kiama.attribution.Attributable._
    import kiama.attribution._

    // Created by parser
    abstract sealed case class LINQAttr() extends Attributable   
    
    abstract case class Exp() extends LINQAttr
    
    case class QueryExp (from: FromClause, qbody: QueryBody) extends Exp
      
    case class FromClause (tp: List[String], val variable: String, in: Exp) extends BodyClause
    case class QueryBody (qbclauses: List[BodyClause], selGby: SG, qcont: Option[QueryCont]) extends LINQAttr
    abstract case class BodyClause() extends LINQAttr
    
    case class QueryCont (val variable: String, qbody: QueryBody) extends LINQAttr
    case class LetClause (val lhs: String, rhs: Exp) extends BodyClause
    case class WhereClause (booltest: Exp) extends BodyClause
    
    case class JoinClause (tp: List[String], val innervar: String, innerexp: Exp, lhs: Exp, rhs: Exp) extends BodyClause
    case class JoinIntoClause (jc: JoinClause, val result: String) extends BodyClause
    
    case class OrderByClause (orderings: List[Ordering]) extends BodyClause
    case class Ordering (ord: Exp, direction: Direction.Value) extends LINQAttr
    
    abstract case class SG() extends LINQAttr
    case class SelectClause(selexp: Exp) extends SG 
    case class GroupByClause(resexp: Exp, clusterexp: Exp) extends SG
    
    object Direction extends Enumeration { 
      val ascending = Value("ascending") 
      val descending = Value("descending")
    }
    object EqOper extends Enumeration { 
      val eq = Value("==")
      val ne = Value("!=")
    }
    object RelOper extends Enumeration { 
      val lt = Value("<")
      val le = Value("<=") 
      val gt = Value(">") 
      val ge = Value(">=")
    }
    object AddOper extends Enumeration { 
      val plus = Value("+") 
      val minus = Value("-")
    }
    object MultOper extends Enumeration { 
      val mult = Value("*") 
      val div = Value("/")
      val modulo = Value("%")
    }
    object UnaryOper extends Enumeration { 
      val neg = Value("!")
      val minus = Value("-")
    }
    
    abstract case class LogicalExp(rands : List[Exp]) extends Exp { def nodeName() : String }
    case class OrExp(override val rands : List[Exp]) extends LogicalExp(rands) { override def nodeName() = "OrExp" }
    case class AndExp(override val rands : List[Exp]) extends LogicalExp(rands) { override def nodeName() = "AndExp" }
    abstract case class BinaryOp(lhs: Exp, rhs: Exp) extends Exp { def nodeName() : String; def ratorName() : String }
    case class EqExp(override val lhs: Exp, override val rhs: Exp, rator: EqOper.Value) extends BinaryOp(lhs, rhs) { 
      override def nodeName() = "EqExp"; override def ratorName() = "==" }
    case class RelExp(override val lhs: Exp, override val rhs: Exp, rator: RelOper.Value) extends BinaryOp(lhs, rhs) { 
      override def nodeName() = "RelExp"; override def ratorName() = rator.toString  }
    case class AddExp(override val lhs: Exp, override val rhs: Exp, rator: AddOper.Value) extends BinaryOp(lhs, rhs) { 
      override def nodeName() = "AddExp"; override def ratorName() = rator.toString }
    case class MultExp(override val lhs: Exp, override val rhs: Exp, rator: MultOper.Value) extends BinaryOp(lhs, rhs) { 
      override def nodeName() = "MultExp"; override def ratorName() = rator.toString  }
    case class UnaryExp(oper: UnaryOper.Value, rand : Exp) extends Exp
    case class Stmt(dotseparated: List[Exp]) extends Exp
    abstract case class PrimExp() extends Exp { def nodeName() : String }
    case class NestedExp(exp: Exp) extends PrimExp { override def nodeName() = "NestedExp" }
    case class MethodCall(methodName: String, optioncast: Option[List[String]], args: List[Exp]) extends PrimExp { override def nodeName() = "MethodCall" }
    abstract case class NewExp(rhss: List[Exp]) extends PrimExp
    case class NewExpTraditional(tp: List[String], override val rhss: List[Exp]) extends NewExp(rhss) { 
      override def nodeName() = "NewExpTraditional" }
    case class NewExpWithInit(tp: Option[List[String]], lhss: List[Option[String]], override val rhss: List[Exp]) extends NewExp(rhss) { 
      override def nodeName() = "NewExpWithInit" }
    abstract case class ValueExp(value: Any) extends PrimExp { override def nodeName() = "ValueExp" }
    case class IntLiteral(override val value: Int) extends ValueExp(value) { override def nodeName() = "IntLiteral"; override def toString() = value.toString }
    case class DoubleLiteral(override val value: Double) extends ValueExp(value) { override def nodeName() = "DoubleLiteral"; override def toString() = value.toString }
    case class StringLiteral(override val value: String) extends ValueExp(value) { override def nodeName() = "StringLiteral"; override def toString() = value.toString }
    case class BoolLiteral(override val value: Boolean) extends ValueExp(value) { override def nodeName() = "BoolLiteral"; override def toString() = value.toString }
    case class UseExp(ident : String) extends ValueExp(ident) { override def nodeName() = "UseExp"; override def toString() = ident }
    
}

abstract class Visitor[T]() {
  
  import AbstractSyntax._
  
   def visit(q: QueryExp, rfrom: T, rqbody: T) : T
   def visit(f: FromClause, rin: T) : T 
   def visit(qb: QueryBody, rqbclauses: List[T], rselGby: T, rqcont: Option[T]) : T
   def visit(qc: QueryCont, rqbody: T) : T
   def visit(lc: LetClause, rrhs: T) : T
   def visit(wc: WhereClause, rbooltest: T) : T
   def visit(jc: JoinClause, rinnerexp: T, rlhs: T, rrhs: T) : T
   def visit(jic: JoinIntoClause, rjc: T) : T 
   def visit(obc: OrderByClause, rorderings: List[T]) : T 
   def visit(oc: Ordering, rord: T) : T 
   def visit(sc: SelectClause, rselexp : T) : T  
   def visit(gbc: GroupByClause, rresexp: T, rclusterexp: T) : T
   def visit(le: LogicalExp, rrands: List[T]) : T
   def visit(bo: BinaryOp, rrhs: T, rlhs: T) : T
   def visit(ue: UnaryExp, rexp: T) : T
   def visit(stmt: Stmt, rdotseparated: List[T]) : T
   def visit(ne: NestedExp, rexp: T) : T
   def visit(mc: MethodCall, rargs: List[T]) : T 
   def visit(ve: ValueExp) : T
   def visit(net: NewExpTraditional, rhss: List[T]) : T
   def visit(newi: NewExpWithInit, rhss: List[T]) : T
}

class Walker[T](v : Visitor[T], defaultRetVal: T) {
  
  import AbstractSyntax._
  
  def walk(e : PrimExp) : T = e match {

   case e @ NestedExp(exp) => {
     val rexp = this walk exp
     v.visit(e, rexp)
   }
   
   case e @ ValueExp(value) => {
     v.visit(e)
   }
   
   case e @ MethodCall(methodname, optioncast, args) => {
     val rargs = args map ( o => this walk o )
     v.visit(e, rargs)
   }
   
   case e @ NewExpTraditional(tp, rhss) => {
     val rrhss = rhss map ( o => this walk o )
     v.visit(e, rrhss)
   }
   
   case e @ NewExpWithInit(tp, lhss, rhss) => {
     val rrhss = rhss map ( o => this walk o )
     v.visit(e, rrhss)
   }

  }
  
  def walk(e : sts.linq.AbstractSyntax.Exp) : T = e match {
    
    case e @ PrimExp() => walk(e)
    
    case e @ QueryExp(from, qbody) => { val rfrom = this walk from; val rqbody = this walk qbody; v.visit(e, rfrom, rqbody) }
    
    case e @ LogicalExp(rands) => {
      val rrands = rands map ( o => this walk o )
      v.visit(e, rrands)
    }
    
   case e @ BinaryOp(lhs, rhs) => {
     val rlhs = this walk lhs
     val rrhs = this walk rhs
     v.visit(e, rlhs, rrhs)
   }
    
   case e @ UnaryExp(oper, rand) => {
     val rrand = this walk rand
     v.visit(e, rrand)
   }
    
   case e @ Stmt(dotseparated) => {
     val rdotseparated = dotseparated map ( o => this walk o )
     v.visit(e, rdotseparated)
   }
    
  } 
  
  def walk(e : BodyClause) : T = e match {
    
    case e @ FromClause(tp, variable, in) => { val rin = this walk in; v.visit(e, rin) }
    
    case e @ LetClause(lhs, rhs) => {
      val rrhs = this walk rhs
      v.visit(e, rrhs)
    }
    
    case e @ WhereClause(booltest) => {
      val rbooltest = this walk booltest
      v.visit(e, rbooltest)
    }
    
    case e @ JoinClause(tp, innervar, innerexp, lhs, rhs) => {
      val rinnerexp = this walk innerexp 
      val rlhs = this walk lhs
      val rrhs = this walk rhs
      v.visit(e, rinnerexp, rlhs, rrhs)
    }
    
    case e @ JoinIntoClause(jc, result) => {
      val rjc = this walk jc
      v.visit(e, rjc) 
    }
    
    case e @ OrderByClause(orderings) => {
      val rorderings = orderings map ( o => this walk o )
      v.visit(e, rorderings)
    }
    
  } 
  
  def walk(e : LINQAttr) : T =  e match {
    
    case e @ BodyClause() => walk(e) 
    case e @ sts.linq.AbstractSyntax.Exp() => walk(e)
    
    case e @ QueryBody(qbclauses, selGby, qcont) => { 
      val rqbclauses = qbclauses map {this walk _}; 
      val rselGby = this walk selGby;
      val rqcont : Option[T] = qcont match { 
        case Some(qcont2) => Some(this walk qcont2)
        case None => None
      } 
      v.visit(e, rqbclauses, rselGby, rqcont) 
    }
    
    case e @ QueryCont(variable, qbody) => {
      val rqbody = this walk qbody
      v.visit(e, rqbody)
    }
    
   
    case e @ Ordering(ord, direction) => {
      val rord = this walk ord
      v.visit(e, rord)
    }
    
    case e @ SelectClause(selexp) => {
      val rselexp = this walk selexp
      v.visit(e, rselexp)
    }
    
    case e @ GroupByClause(resexp, clusterexp) => {
      val rresexp = this walk resexp
      val rclusterexp = this walk clusterexp
      v.visit(e, rresexp, rclusterexp)
    }
    
   case _ => defaultRetVal
  }
  
}

