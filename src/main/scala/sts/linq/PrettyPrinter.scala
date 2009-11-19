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

object PrettyPrinter extends Visitor[List[String]] {
  
  val sb = new StringBuilder()
  var currentindent = 0
  def blanks(n : Int) : String =  {
    val output = new StringBuilder(n);
    (1 to n) foreach { _ => output.append(' ') }
    output.toString() 
  }
  val EMPTY = "<empty>"
  def indent() = sb.append(blanks(currentindent))
  
  import scala.collection.mutable.ListBuffer
  
  def pretty(e: LINQAttr) : String = {
    val w = new Walker(this, List());
    val lines = w walk e
    val res = new StringBuffer();
    lines foreach { line : String => res.append(line + '\n') }
    res.toString
  } 
  
   def visit(q: QueryExp, rfrom: List[String], rqbody: List[String]) = {
     val pp = new PP("QueryExp")
     pp.addSubtree("from", rfrom)
     pp.addSubtree("qbody", rqbody)
     pp.toStringList
   } 
     
   def visit(f: FromClause, rin: List[String]) = {
     val pp = new PP("FromClause")
     pp.addLabel("tp", f.tp.mkString("."))
     pp.addLabel("variable", f.variable)
     pp.addSubtree("in", rin)
     pp.toStringList
   } 

   def visit(qb: QueryBody, rqbclauses: List[List[String]], rselGby: List[String], rqcont: Option[List[String]]) = {
     val pp = new PP("QueryBody")
     pp.addSeveralSubtree("qbclause", rqbclauses)
     pp.addSubtree("selGby", rselGby)
     pp.addSubtree("qcont", rqcont)
     pp.toStringList
   }
   
   def visit(qc: QueryCont, rqbody: List[String]) = {
     val pp = new PP("QueryCont")
     pp.addLabel("variable", qc.variable)
     pp.addSubtree("rqbody", rqbody)
     pp.toStringList
   }
   
   def visit(lc: LetClause, rrhs: List[String]) = {
     val pp = new PP("LetClause")
     pp.addLabel("lhs", lc.lhs)
     pp.addSubtree("rhs", rrhs)
     pp.toStringList
   }
   
   def visit(wc: WhereClause, rbooltest: List[String]) = {
     val pp = new PP("WhereClause")
     pp.addSubtree("booltest", rbooltest)
     pp.toStringList
   }
   
   def visit(jc: JoinClause, rinnerexp: List[String], rlhs: List[String], rrhs: List[String]) = {
     val pp = new PP("JoinClause")
     pp.addLabel("tp", jc.tp.mkString("."))
     pp.addLabel("innervar", jc.innervar)
     pp.addSubtree("innerexp", rinnerexp)
     pp.addSubtree("rlhs", rlhs)
     pp.addSubtree("rrhs", rrhs)
     pp.toStringList
   }
   
   def visit(jic: JoinIntoClause, rjc: List[String]) = {
     val pp = new PP("JoinIntoClause")
     pp.addSubtree("jc", rjc)
     pp.addLabel("result", jic.result)
     pp.toStringList
   }
    
   def visit(obc: OrderByClause, rorderings: List[List[String]]) = {
     val pp = new PP("OrderByClause")
     pp.addSeveralSubtree("ordering", rorderings)
     pp.toStringList
   }
    
   def visit(oc: Ordering, rord: List[String]) = {
     val pp = new PP("Ordering")
     pp.addSubtree("ord", rord)
     pp.addLabel("direction", oc.direction.toString)
     pp.toStringList
   }
    
   def visit(sc: SelectClause, rselexp : List[String]) = {
     val pp = new PP("SelectClause")
     pp.addSubtree("selexp", rselexp)
     pp.toStringList
   }
   
   def visit(gbc: GroupByClause, rresexp: List[String], rclusterexp: List[String]) = {
     val pp = new PP("GroupByClause")
     pp.addSubtree("resexp", rresexp)
     pp.addSubtree("rclusterexp", rclusterexp)
     pp.toStringList
   }
   
   def visit(le: LogicalExp, rrands: List[List[String]]) = {
     val pp = new PP(le.nodeName())
     pp.addSeveralSubtree("rand", rrands)
     pp.toStringList
   }
   
   def visit(bo: BinaryOp, rlhs: List[String], rrhs: List[String]) = {
     val pp = new PP(bo.nodeName())
     pp.addSubtree("lhs", rlhs)
     pp.addLabel("oper", bo.ratorName())
     pp.addSubtree("rhs", rrhs)
     pp.toStringList
   }
   
   def visit(ue: UnaryExp, rexp: List[String]) = {
     val pp = new PP("UnaryExp")
     pp.addSubtree("exp", rexp)
     pp.toStringList
   }
   
   def visit(stmt: Stmt, rdotseparated: List[List[String]]) = {
     val pp = new PP("Stmt")
     pp.addSeveralSubtree("dot", rdotseparated)
     pp.toStringList
   }
   
   def visit(ne: NestedExp, rexp: List[String]) = {
     val pp = new PP("NestedExp")
     pp.addSubtree("rexp", rexp)
     pp.toStringList
   }
   
   def visit(ve: ValueExp) = {
     val pp = new PP(ve.nodeName)
     pp.addLabel("value", ve.value.toString)
     pp.toStringList
   }
   
   def visit(mc: MethodCall, rargs: List[List[String]]) = {
     val pp = new PP("MethodCall")
     pp.addLabel("methodName", mc.methodName)
     pp.addLabel("optioncast", mc.optioncast match { case Some(cast) => cast.head; case None => "" })
     pp.addSeveralSubtree("arg", rargs)
     pp.toStringList
   }
   
   def visit(net: NewExpTraditional, rrhss: List[List[String]]) = {
     val pp = new PP("NewExpTraditional")
     pp.addLabel("tp", net.tp.mkString("."))
     pp.addSeveralSubtree("rhs", rrhss)
     pp.toStringList
   }

   def visit(net: NewExpWithInit, rrhss: List[List[String]]) = {
     val pp = new PP("NewExpWithInit")
     pp.addLabel("tp", net.tp)
     val inits = net.lhss zip rrhss
     inits foreach { case (lhs, rhs) => val lhs2 = lhs.toString; pp.addSubtree(lhs2, rhs) }
     pp.toStringList
   }

   private class PP(nodeType : String) {
     val res = new ListBuffer[String]
     var firstSubtreeAdded = false
     val nodeTypeIndent = blanks(nodeType.length) + "| "
     def addSubtree(label : String, block : List[String]) {
       if (block.isEmpty) 
         addLabel(label, "")
       else {
         var msg = label + " = "
         if (firstSubtreeAdded) 
           res += nodeTypeIndent + msg + block.first
         else {
           firstSubtreeAdded = true
           res += nodeType + "| " + msg + block.first
         }
         if (block.size > 1) {
           val blockIndent = nodeTypeIndent  + blanks(msg.length)
           ((block tail) init) foreach { line : String =>  res append (blockIndent + line )}
           val lastLine = ((block tail) last) 
           res append (blockIndent + lastLine) 
         }
       }
     }
     def addSubtree(label : String, block : Option[List[String]]) {
       block match {
         case Some(b) => addSubtree(label, b)
         case None => addLabel(label, "")
       }
     }
     def addLabel(label : String, msg : Option[List[String]]) {
       msg match {
         case Some(msg2) => { addLabel(label, msg2.mkString(".")) }
         case None => { addLabel(label, "") }
       }
     }
     def addLabel(label : String, msg : String) {
       var msg2 = if (msg.isEmpty) EMPTY else msg
       if (firstSubtreeAdded) 
         res += nodeTypeIndent + label + " = " + msg2
       else {
         firstSubtreeAdded = true
         res += nodeType + "| " + label + " = " + msg2
       }
     }
     def addSeveralSubtree(label: String, blocks: List[List[String]]) {
       val total = blocks.length
       val r = (1 to total).toList
       val toIter = blocks zip r
       toIter foreach {
           arg => { val msg = label + "(" + arg._2 + "of" + total +")"; addSubtree(msg , arg._1); } 
       }
     }
     def toStringList = res.toList
   }
}



class Stringify extends Visitor[String] {
   def apply(e: LINQAttr) : String = {
    val w = new Walker[String](this, "TODO");
    w walk e
  } 
  def tpVar(tp: List[String], variable: String) = variable + (if (tp.isEmpty) "" else (" : " + tp.mkString(".")))
  def paren(str: String) = "( " + str + " )"  
   
   def visit(q: QueryExp, rfrom: String, rqbody: String) = rfrom + rqbody 
   def visit(f: FromClause, rin: String) =  "from " + tpVar(f.tp, f.variable) + " in " + paren(rin)
   def visit(qb: QueryBody, rqbclauses: List[String], rselGby: String, rqcont: Option[String]) = 
     rqbclauses.mkString("\n") + " " + rselGby + rqcont.getOrElse("") 
   def visit(qc: QueryCont, rqbody: String) = " into " + qc.variable + " " + rqbody
   def visit(lc: LetClause, rrhs: String) = "let " + lc.lhs + " = " + rrhs
   def visit(wc: WhereClause, rbooltest: String) = "where " + rbooltest
   def visit(jc: JoinClause, rinnerexp: String, rlhs: String, rrhs: String) =
     "join " + tpVar(jc.tp, jc.innervar) + " in " + rinnerexp + " on " + rlhs + " equals " + rrhs 
   def visit(jic: JoinIntoClause, rjc: String) = rjc + " into " + jic.result  
   def visit(obc: OrderByClause, rorderings: List[String]) = "orderby " + rorderings.mkString(", ") 
   def visit(oc: Ordering, rord: String) = rord + oc.direction
   def visit(sc: SelectClause, rselexp : String) = "select " + rselexp 
   def visit(gbc: GroupByClause, rresexp: String, rclusterexp: String) = "group " + rresexp + " by " + rclusterexp 
   def visit(le: LogicalExp, rrands: List[String]) = (rrands map {paren(_)}).mkString(le match { case AndExp(_) => "&&"; case OrExp(_) => "||"})
   def visit(bo: BinaryOp, rrhs: String, rlhs: String) = rrhs + " " + bo.ratorName() + " " + rlhs 
   def visit(ue: UnaryExp, rexp: String) = ue.oper + rexp 
   def visit(stmt: Stmt, rdotseparated: List[String]) = rdotseparated.mkString(".")
   def visit(ne: NestedExp, rexp: String) = "(" + rexp +  ")"
   def visit(mc: MethodCall, rargs: List[String]) = {
     val mname = mc.optioncast match { case Some(cast) => "Cast"; case _ => mc.methodName  }
     mname + "(" + rargs.mkString(", ") +  ")"
   } 
   def visit(ve: ValueExp) = ve.value.toString
   def visit(net: NewExpTraditional, rrhss: List[String]) = "new " + net.tp.mkString(".") + " (" + rrhss.mkString(", ") + ")" 
   def visit(newi: NewExpWithInit, rhss: List[String]) = {
     val strtp = newi.tp match { case Some(ts) => ts.mkString("."); case _ => ""  }
     val fields = (newi.lhss zip rhss) map { t => (if (t._1.isEmpty) "" else t._1.get + " = " ) + t._2 } 
     "new " + strtp + " { " + fields.mkString(", ") + " } "
   } 
}

object Stringify  extends Stringify {}

class ComprehendWalker(v : Comprehend, defaultRetVal: String) extends Walker[String](v, defaultRetVal) {
   override def walk(e : BodyClause) : String = e match {
    
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
      val rinnerexp = this walk jc.innerexp 
      val rlhs = this walk jc.lhs
      val rrhs = this walk jc.rhs
      v.visitJIC(e, rinnerexp, rlhs, rrhs) 
    }
    
    case e @ OrderByClause(orderings) => {
      val rorderings = orderings map ( o => this walk o )
      v.visit(e, rorderings)
    }
    
  }
 
}
   


class Comprehend extends Stringify {
  // TODO should be a really fresh name
  def fresh(suggestion: String) = suggestion
  
  override def apply(e: LINQAttr) : String = {
    val w = new ComprehendWalker(this, "TODO")
     w walk e
  }
  
  override def visit(qe: QueryExp, rfrom: String, rqbody: String) = {

    val transformed = Transformer inlinecontinuations qe
    val linearized = Linearize(transformed.asInstanceOf[QueryExp])
    val res = new StringBuffer();
    
  def noOrderBy() = {
    res.append("for (")
    val comprqualifiers = linearized.clauses map { Comprehend(_) }
    res.append(comprqualifiers.mkString("; "))
    res.append(") yield " + Comprehend(linearized.selGby))
    linearized.selGby match {
      case sg @ SelectClause(_) => paren(res.toString)
      case sg @ GroupByClause(_,_) => {
        val pairseq = paren(res.toString)
        val keySelector = "_._1"
        val resultSelector = "_._2"
        paren(pairseq + " groupBy (" + keySelector + ", " + resultSelector + ")")
      }
    }
  }
 
  def withOrderBy() = {
    // TODO as of now assumes all sort directions to be ascending (i.e., sortBy works that way)
    val (clausesBefore , clausesWithAndAfter) = linearized.clauses span (!isOrderByClause(_))
    val clauseOrderBy = clausesWithAndAfter.head
    val clausesAfter = clausesWithAndAfter.tail
    val receiverTuple = paren(linearized.visibleVars(clauseOrderBy).mkString(", ")) 
    val innerForQualis = clausesBefore map { Comprehend(_) }
    val innerFor = "for ( " + innerForQualis.mkString("; ") + " ) yield " + receiverTuple
    val sortCriteria = receiverTuple + " =>"  + Comprehend(clauseOrderBy) 
    val sortedTuples = paren(innerFor) + " sortBy " + paren(sortCriteria)
    val againInScope = receiverTuple + " <- " + paren(sortedTuples)
    // same as for noOrderBy, only that for clausesAfter
    res.append("for (")
    res.append(againInScope)
    val afterQualis = clausesAfter map { Comprehend(_) }
    if(!afterQualis.isEmpty){
      res.append(";")
    }
    res.append(afterQualis.mkString("; "))
    res.append(") yield " + Comprehend(linearized.selGby))
    linearized.selGby match {
      case sg @ SelectClause(_) => paren(res.toString)
      case sg @ GroupByClause(_,_) => {
        val pairseq = paren(res.toString)
        val keySelector = "_._1"
        val resultSelector = "_._2"
        paren(pairseq + " groupBy (" + keySelector + ", " + resultSelector + ")")
      }
    }
    
  }
  
  def isOrderByClause(c: BodyClause) = c match { case OrderByClause(_) => true; case _ => false }  
    
    if (linearized.clauses exists ( isOrderByClause _ )) 
      withOrderBy()
    else
      noOrderBy()

 
  }
  
  override  def visit(fc: FromClause, rin: String) =  tpVar(fc.tp, fc.variable) + " <- " + rin
  override  def visit(lc: LetClause, rrhs: String) = "val " + lc.lhs + " = " + rrhs
  override  def visit(wc: WhereClause, rbooltest: String) = "if " + paren(rbooltest)
  override  def visit(jc: JoinClause, rinnerexp: String, rlhs: String, rrhs: String) = {
     val outerKey = fresh("outerKey")
     "val " + outerKey  + " = " + rlhs + " ; " + 
     tpVar(jc.tp, jc.innervar) + " <- " + rinnerexp +  
     " ; if " + outerKey + " == " + rrhs 
   }
   def visitJIC(jic: JoinIntoClause, rinnerexp: String, rlhs: String, rrhs: String) = {
     val outerKey = fresh("outerKey")
     val innerItems = tpVar(jic.jc.tp, jic.jc.innervar) + " <- " + rinnerexp + " ; if " + outerKey + " == " + rrhs 
     val group = "for ( " + innerItems + " ) yield " + jic.jc.innervar
     "val " + outerKey  + " = " + rlhs + " ; " +
     "val " + jic.result + " = " + paren(group)  
   }
  override def visit(obc: OrderByClause, rorderings: List[String]) = paren(rorderings.mkString(", ")) 
  override def visit(oc: Ordering, rord: String) = rord 
  override def visit(sc: SelectClause, rselexp : String) = rselexp 
  override def visit(gbc: GroupByClause, rresexp: String, rclusterexp: String) = "(" + rclusterexp + ", " + rresexp + ")" 
  override /*TODO Cast -> asInstanceOf */  def visit(mc: MethodCall, rargs: List[String]) = {
     val mname = mc.optioncast match { case Some(cast) => "Cast<"+cast.mkString(".")+">"; case _ => mc.methodName  }
     mname + "(" + rargs.mkString(", ") +  ")"
   } 
  override def visit(newi: NewExpWithInit, rhss: List[String]) = {
     val strtp = newi.tp match { case Some(ts) => ts.mkString("."); case _ => ""  }
     val fields = (newi.lhss zip rhss) map { t => (if (t._1.isEmpty) "" else t._1.get + " = " ) + t._2 } 
     "new " + strtp + " { val " + fields.mkString("; val ") + " } "
   }
  
  override def visit(stmt: Stmt, rdotseparated: List[String]) = rdotseparated.mkString(".")
}

object Comprehend extends Comprehend {} 