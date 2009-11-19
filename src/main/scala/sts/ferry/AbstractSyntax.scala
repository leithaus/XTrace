/******************************************************************************* 
 * This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 *     Anastasia Izmaylova, 
 *     Miguel Garcia, http://www.sts.tu-harburg.de/people/mi.garcia 
 *******************************************************************************/ 


package sts.ferry

object AbstractSyntax {
  
  // TODO create text file with AST hierarchy
  
  // case classes representing Ferry type system
  
  case class Ft
  case class Fb extends Ft
  // special tuple that represents Ferry tuple type and consider propagation of column names
  
  // under assumption of flattening, the following check is commented
  // under aforementioned assumption, Ft is considered instead of Fb
  
  /*
  case class FTuple(cols: List[Option[String]], elemTs: List[Fb]) extends Ft {
    
    type ColDef = Tuple2[Option[String],Fb]
    
    // STYLE case class ColDef2(cname : Option[String], ctype : Fb)
    
    def apply (pos: int): ColDef = {
      require(pos < elemTs.size)
      (cols(pos),elemTs(pos))
    } 
    
    def apply (col: String): Option[Tuple2[Option[String],Fb]] = {
      var i = 0
      var n = 0
      var tpt: Fb = null
      for (c <- cols) {
        c match {
          case Some(cc) => if (cc==col) { i = i + 1; n = n + 1; tpt = elemTs(i) }
          case None => i = i + 1
        }
      }
      if (n > 1) throw new Error("Duplication of column names is found")
      if (n == 0) throw new Error("No such column name is found")
      return Some((Some(col),tpt))
    }
    
    override def toString = {
      var str = "("
      var i = 0
      for (col <- cols) {
        col match {
          case Some(c) => 
            if (i != elemTs.size - 1) {
              str = str + "'" + c + "'" + " " + elemTs(i) + ", "; i = i + 1
            } else {
            	str = str + "'" + c + "'" + " " + elemTs(i); i = i + 1
            }
          case None => 
            if (i != elemTs.size - 1) {
              str = str + elemTs(i) + ", "; i = i + 1
            } else {
            	str = str + elemTs(i); i = i + 1
            }
        }  
      }
      str = str + ")"
      str
    }
    
  }
  */
  case class FTuple(cols: List[Option[String]], elemTs: List[Ft]) extends Ft {
    
    type ColDef = Tuple2[Option[String],Ft]
    
    // STYLE case class ColDef2(cname : Option[String], ctype : Fb)
    
    def apply (pos: int): ColDef = {
      require(pos < elemTs.size)
      (cols(pos),elemTs(pos))
    } 
    
    def apply (col: String): Option[Tuple2[Option[String],Ft]] = {
      var i = 0
      var n = 0
      var tpt: Ft = null
      for (c <- cols) {
        c match {
          // updated
          case Some(cc) => if (cc==col) { tpt = elemTs(i); n = n + 1;}; i = i + 1; 
          case None => i = i + 1
        }
      }
      if (n > 1) throw new Error("Duplication of column names is found")
      if (n == 0) throw new Error("No such column name is found")
      return Some((Some(col),tpt))
    }
    
    override def toString = {
      var str = "("
      var i = 0
      for (col <- cols) {
        col match {
          case Some(c) => 
            if (i != elemTs.size - 1) {
              str = str + "'" + c + "'" + " " + elemTs(i) + ", "; i = i + 1
            } else {
            	str = str + "'" + c + "'" + " " + elemTs(i); i = i + 1
            }
          case None => 
            if (i != elemTs.size - 1) {
              str = str + elemTs(i) + ", "; i = i + 1
            } else {
            	str = str + elemTs(i); i = i + 1
            }
        }  
      }
      str = str + ")"
      str
    }
    
  }
  //------------------------------------------------------------------------------------------
  case class FList(elemT: Ft) extends Fb {
    override def toString = {
      "[" + elemT + "]" 
    }
  }
  
  object Atom extends Enumeration {
    val Fbool = Value("boolean")
    val Fint = Value("int")
    val Fdouble = Value("double")
    val Fstring = Value("string")
  }
  
  case class Fa(elemT: Atom.Value) extends Fb {
    override def toString = {
      elemT.toString
    }
  }
  
  // case classes representing Ferry grammar
  
  // in the case of generating a Ferry parser by using Kiama projects
  abstract sealed case class FerryAttr /* extends Attributable */ {
    private[this] var envv: List[VarIDDecl] = List()
    def addToEnv(e: VarIDDecl) = { envv = envv:::List(e) }
    def env: List[VarIDDecl] = envv
    def env_=(en: List[VarIDDecl]) {envv = en}
  }
  
  abstract case class Expr extends FerryAttr {
    
    private[this] var rawtpt: Ft = _
    def tpt:Ft = rawtpt
    def tpt_=(t: Ft){rawtpt = t}
    
  }
  
  abstract case class LiteralExpr(literal: Any) extends Expr {override def toString = literal.toString}
  
  case class IntLiteral(override val literal: Int) extends LiteralExpr(literal) {
    tpt_=(Fa(Atom.Fint))
  }
  case class StringLiteral(override val literal: String) extends LiteralExpr(literal) {
    tpt_=(Fa(Atom.Fstring))
    override def toString = "'" + literal.toString + "'"
  }
  case class BoolLiteral(override val literal: Boolean) extends LiteralExpr(literal) {
    tpt_=(Fa(Atom.Fbool))
  }
  
  object UnOp extends Enumeration {
    
    val FNot = Value("not")
    
  }
  
  object BinOp extends Enumeration {
    
    val Fadd = Value("+")
    val Fminus = Value("-")
    
    val Fmult = Value("*")
    val Fdiv = Value("/")
    
    val Feq = Value("==")
    val Fnq = Value("!=")
    
    val Flt = Value("<")
    val Fle = Value("<=")
    val Fgt = Value(">")
    val Fge = Value(">=")
    
    val FAnd = Value("and")
    val FOr = Value("or")
  }
  
  abstract case class OpAppExpr() extends Expr() {}
  
  case class UnOpExpr (expr: Expr, op: UnOp.Value) extends OpAppExpr {
    require(op != null)
    override def toString = op.toString + " " + expr
  }
  
  case class BinOpExpr (lhs: Expr, rhs: Expr, op: BinOp.Value) extends OpAppExpr {
    require(op != null)
    override def toString = lhs + " " + op.toString + " " + rhs
  }
  
  case class TupleExpr(exprs:List[Expr]) extends Expr {
    require (exprs != null)
    require (exprs.size >= 2)
    override def toString = {
      var str = "("
      for (i <- 0 to exprs.size - 1) { 
          if (i != exprs.size - 1) {
            str = str + exprs(i) + ", "
          } else {
            str = str + exprs(i) + ")"
          }
      } 
      str
    } 
  }
  
  // custom constructor allowing records for Ferry
  case class RecordExpr (cols: List[TableColID], exprs:List[Expr]) extends Expr {
    require (cols != null)
    require (exprs != null)
    require (exprs.size >= 2)
    require (cols.length == exprs.length)
    override def toString = {
      var str = "("
      for (i <- 0 to exprs.size - 1) { 
          if (i != exprs.size - 1) {
            str = str + cols(i) + " " + exprs(i) + ", "
          } else {
            str = str + cols(i) + " " + exprs(i) + ")"
          }
      } 
      str
    } 
  }
  
  case class NatLiteral(literal: Int) extends FerryAttr { require( literal >= 0 ); override def toString = literal.toString }
  
  case class PosAccExpr(qualifier:Expr, selector: NatLiteral) extends Expr {
    require (qualifier != null)
    require (selector != null)
    override def toString = qualifier + "." + selector
  }
  
  case class ListExpr(exprs:List[Expr]) extends Expr {
    require (exprs != null)
    override def toString = {
      var str = "["
      for (i <- 0 to exprs.size-1) { 
        if (i != exprs.size-1) {
          str = str + exprs(i) + ","
        } else {
          str = str + exprs(i)
        }
      } 
      str = str + "]"
      str
    }
  }
  
  /*
     A variable declaration
   */
  case class VarIDDecl (id: String) extends FerryAttr { 
    require (id != null)
    // type of the expression evaluated in the binding environment
    private[this] var bexpr: Expr = _
    def expr:Expr = bexpr
    def expr_=(e: Expr){bexpr = e}
    
    private[this] var btpt: Ft = _
    def tpt:Ft = btpt
    def tpt_=(t: Ft){btpt = t}
    
    override def toString = id
  }
  
  /*
     A variable usage
   */
  case class VarUseExpr(id: VarIDDecl) extends Expr { 
    require (id != null)
    override def toString = id.toString 
  }
  
  case class LetBindingClause(bindings: List[Tuple2[VarIDDecl,Expr]]) extends FerryAttr {
    require (bindings != null)
    require (!bindings.isEmpty)
    
    for (bind <- bindings) {
      bind._1.expr_=(bind._2)
    }
    
    override def toString = {
      var str = ""
      for (i <- 0 to bindings.size - 1) {
        if (i != bindings.size - 1) {
          str = str  + bindings(i)._1.toString + " = " + bindings(i)._2.toString + ", "
        } else {
          str = str  + bindings(i)._1.toString + " = " + bindings(i)._2.toString
        }
        
      }
      str
    }
  }
  case class LetExpr(bindings:LetBindingClause, body:Expr) extends Expr {
    require (bindings != null)
    require (body != null)
    override def toString = "let " + bindings.toString + " in " + body
  }
  
  case class IfExpr(cond:Expr, thencls:Expr, elsecls:Expr) extends Expr {
    require (cond != null)
    require (thencls != null)
    require (elsecls != null)
    override def toString = "if " + cond + " then " + thencls + " else " + elsecls
  }
  
  case class ForBindingClause(bindings: List[Tuple2[VarIDDecl,Expr]]) extends FerryAttr {
    require (bindings != null)
    require (!bindings.isEmpty)
    
    for (bind <- bindings) {
      bind._1.expr_=(bind._2)
    }
    
    override def toString = {
      var str = ""
      for (i <- 0 to bindings.size - 1) {
        if (i != bindings.size - 1) {
          str = str  + bindings(i)._1.toString + " in " + bindings(i)._2.toString + ", "
        } else {
          str = str  + bindings(i)._1.toString + " in " + bindings(i)._2.toString
        }
        
      }
      str
    }
  }
  case class WhereClause(expr: Expr) extends FerryAttr {
    require (expr != null)
    override def toString = " where " + expr
  }
  case class GroupByClause(exprs:List[Expr]) extends FerryAttr {
    require (exprs != null)
    require (!exprs.isEmpty)
    override def toString = {
      var str = " group by "
      for (i <- 0 to exprs.size - 1) { 
          if (i != exprs.size - 1) {
            str = str + exprs(i) + ", "
          } else {
            str = str + exprs(i)
          }
      } 
      str
    }
  }
  
  object OrderModifier extends Enumeration {
    val Fascending = Value("ascending")
    val Fdescending = Value("descending")
  }
  
  case class OrderSpec (expr: Expr, modifier: OrderModifier.Value) extends FerryAttr {
    require (expr != null)
    override def toString: String = {
      if (modifier != null) {
        return expr.toString + " " + modifier.toString
      } else {
        return expr.toString
      }
    }
  }
  case class OrderByClause (orderspecs:List[OrderSpec]) extends FerryAttr {
    require (orderspecs != null)
    require (!orderspecs.isEmpty)
    override def toString = {
      var str = " order by "
      for (i <- 0 to orderspecs.size - 1) {
        if (i != orderspecs.size - 1) {
            str = str + orderspecs(i) + ", "
         } else {
           str = str + orderspecs(i)
         }
      } 
      str
    }
  }
  
  case class ForExpr(bindings: ForBindingClause, wherecls: Option[WhereClause], groupbycls: Option[GroupByClause], groupbywherecls: Option[WhereClause], orderbycls: Option[OrderByClause], expr: Expr) extends Expr {
    require (bindings != null)
    require (expr != null)
    override def toString = {
      var str = "for " + bindings
      wherecls match {
        case Some(cls) => str = str + " " + cls
        case None =>
      }
      groupbycls match {
        case Some(cls) => str = str + " " + cls
        case None =>
      } 
      groupbywherecls match {
        case Some(cls) => str = str + " " + cls
        case None =>
      }
      orderbycls match {
        case Some(cls) => str = str + " " + cls
        case None =>
      }
      str = str + " return " + expr
      str
    }
  }
  
  case class TableID (id: String) extends FerryAttr {
    require (id != null)
    override def toString = id
  }
  
  case class TableColID (id: String) extends FerryAttr {
    require (id != null)
    override def toString = id
  }
  case class TableColSpec (id: TableColID, ctpt: Atom.Value) extends FerryAttr {
    require (id != null)
    require (ctpt != null)
    override def toString = id.toString + " " + ctpt.toString
  }
  case class TableColSpecs (specs: List[TableColSpec]) extends FerryAttr {
    require (specs != null)
    require (!specs.isEmpty)
    override def toString = {
      var str = ""
      for (i <- 0 to specs.size - 1) {
        if (i != specs.size - 1) {
            str = str + specs(i) + ", "
          } else {
            str = str + specs(i)
          }
      } 
      str
    }
  }
  
  case class TableKeySpec (cids: List[TableColID]) extends FerryAttr {
    require (cids != null)
    require (!cids.isEmpty)
    override def toString = {
      var str = "("
      for (i <- 0 to cids.size - 1) { 
         if (i != cids.size - 1) {
            str = str + cids(i) + ", "
          } else {
            str = str + cids(i)
          }
      } 
      str = str + ")"
      str
    }
  }
  case class TableKeySpecs (specs: List[TableKeySpec]) extends FerryAttr {
    require (specs != null)
    require (!specs.isEmpty)
    override def toString = {
      var str = ""
      for (i <- 0 to specs.size - 1){ 
          if (i != specs.size - 1) {
            str = str + specs(i) + ", "
          } else {
            str = str + specs(i)
          }
      } 
      str
    }
  }
  
  case class TableOrderSpec (cid: TableColID, modifier: OrderModifier.Value) extends FerryAttr {
    require (cid != null)
    override def toString: String = {
      if (modifier != null) {
        return cid.toString + " " + modifier.toString
      } else {
        return cid.toString
      }
    }
  }
  
  case class TableOrderSpecs (specs: List[TableOrderSpec]) extends FerryAttr {
    require (specs != null)
    require (!specs.isEmpty)
    override def toString = {
      var str = ""
      for (i <- 0 to specs.size - 1) {
          if (i != specs.size - 1) {
            str = str + specs(i) + ", "
          } else {
            str = str + specs(i)
          }
      } 
      str
    }
  }
  
  case class TableRefExpr(id: TableID, cspecs: TableColSpecs, kspecs: TableKeySpecs, ospecs: Option[TableOrderSpecs]) extends Expr {
    require (id != null)
    require (cspecs != null)
    require (kspecs != null)
    override def toString = {
      var str = " table " + id.toString + " ("+ cspecs.toString + ")"
      str = str + " with keys (" + kspecs.toString + ")"
      if (ospecs != None) {
        str = str + " with order (" + ospecs.toString + ")"
      }
      str
    }
  }
  
  case class NomAccExpr (expr: Expr, selector: TableColID) extends Expr {
    require (expr != null)
    require (selector != null)
    override def toString = expr.toString + "." + selector.toString
  }
  
  object FunIDs extends Enumeration {
    
    val Fappend = Value("append")
    val Fconcat = Value("concat")
    val Ftake = Value("take")
    val Fdrop = Value("drop")
    val Fnth = Value("nth")
    val Funordered = Value("unordered")
    val Flength = Value("length")
    val Fsum = Value("sum")
    val Fmin = Value("min")
    val Fmax = Value("max")
    val Fthe = Value("the")
    val FgroupWith = Value("groupWith")
    
    val Fmap = Value("map")
    val Ffilter = Value("filter")
    val Fzip = Value("zip")
    val Funzip = Value("unzip")
    val FsortBy = Value("sortBy")
    val FgroupBy = Value("groupBy")
    
    // updated: custom operator
    val Frange = Value("range")
    
  }
  
  case class FunID(id: FunIDs.Value) extends FerryAttr {
    require (id != null)
    override def toString = id.toString
  }
  
  case class FunArgs extends FerryAttr {}
  
  case class FunListArgs (exprs: List[Expr]) extends FunArgs {
    require (exprs != null)
    override def toString = {
      var str = ""
      for (i <- 0 to exprs.size - 1) {
          if (i != exprs.size - 1) {
            str = str + exprs(i) + ", "
          } else {
            str = str + exprs(i)
          }
      } 
      str 
    }
      
  }
  case class FunCondArgs (id: VarIDDecl, lexpr: Expr, rexpr: Expr) extends FunArgs {
    require (id != null)
    require (lexpr != null)
    require (rexpr != null)
    id.expr_=(rexpr)
    override def toString = id.toString + " -> " + lexpr.toString + ", " + rexpr.toString
  }
  
  case class FunAppExpr (id: FunID, args: FunArgs) extends Expr {
    require (id != null)
    override def toString: String = {
      if (args != null ) {
        return id.toString + "(" + args.toString + ")"
      } else {
        return id.toString + "()"
      }
    } 
  }
  
  case class ParenthesizedExpr(expr: Expr) extends Expr {
    require (expr != null)
    override def toString = "(" + expr + ")"
  }
  
  abstract class Visitor[T]() {
  
   def visit(e: IntLiteral, literal: Int) : T
   def visit(e: StringLiteral, literal: String) : T
   def visit(e: BoolLiteral, literal: Boolean) : T
   def visit(e: UnOpExpr, expr: T, op: UnOp.Value) : T 
   def visit(e: BinOpExpr, lhs: T, rhs: T, op: BinOp.Value) : T
   def visit(e: TupleExpr, exprs: List[T]) : T
   def visit(e: RecordExpr, cols: List[T], expr: List[T]) : T
   def visit(e: NatLiteral, literal: Int) : T
   def visit(e: PosAccExpr, qualifier: T, selector: T) : T
   def visit(e: ListExpr, exprs: List[T]) : T
   def visit(e: VarIDDecl, id: String) : T
   def visit(e: VarUseExpr, id: T) : T
   def visit(e: LetBindingClause, bindings: List[Tuple2[T,T]]): T
   def visit(e: LetExpr, bindings: T, body: T) : T
   def visit(e: IfExpr, cond: T, thencls: T, elsecls: T) : T
   def visit(e: ForBindingClause, bindings: List[Tuple2[T,T]]) : T
   def visit(e: WhereClause, expr: T) : T
   def visit(e: GroupByClause, exprs: List[T]) : T
   def visit(e: OrderSpec, expr: T, modifier: OrderModifier.Value) : T
   def visit(e: OrderByClause, orderspecs: List[T]) : T
   def visit(e: ForExpr, bindings: T, wherecls: Option[T], groupbycls: Option[T], groupbywherecls: Option[T], orderbycls: Option[T], expr: T) : T
   def visit(e: TableID, id: String) : T
   def visit(e: TableColID, id: String) : T
   def visit(e: TableColSpec, id: T, ctpt: Atom.Value) : T
   def visit(e: TableColSpecs, specs: List[T]) : T
   def visit(e: TableKeySpec, cids: List[T]) : T
   def visit(e: TableKeySpecs, specs: List[T]) : T
   def visit(e: TableOrderSpec, cid: T, modifier: OrderModifier.Value) : T
   def visit(e: TableOrderSpecs, specs: List[T]) : T
   def visit(e: TableRefExpr, id: T, cspecs: T, kspecs: T, ospecs: Option[T]) : T
   def visit(e: NomAccExpr, expr: T, selector: T) : T
   def visit(e: FunID, id: FunIDs.Value) : T
   def visit(e: FunListArgs, expr: List[T]) : T
   def visit(e: FunCondArgs, id: T, lexpr: T, rexpr: T) : T
   def visit(e: FunAppExpr, id: T, args: T) : T
   def visit(e: ParenthesizedExpr, expr: T) : T
   
}

class Walker[T](v : Visitor[T], defaultRetVal: T) {
  
  def walk(e : Expr) : T = e match {
	  
    case e @ IntLiteral (literal) =>
      v.visit(e, literal)
      
    case e @ StringLiteral (literal) =>
      v.visit(e, literal)
      
    case e @ BoolLiteral (literal) =>
      v.visit(e, literal)
      
    case e @ UnOpExpr (expr, op) =>
      expr.env_=(e.env)
      val expr1 = this walk expr
      v.visit(e, expr1, op)
      
    case e @ BinOpExpr (lhs, rhs, op) =>
      Set(lhs, rhs) map (ex => ex.env_=(e.env))
      val lhs1 = this walk lhs
      val rhs1 = this walk rhs
      v.visit(e, lhs1, rhs1, op)
      
    case e @ TupleExpr (exprs) =>
      exprs map (ex => ex.env_=(e.env))
      val exprs1 = exprs map (ex => this walk ex)
      v.visit(e, exprs1)
      
    case e @ RecordExpr (cols, exprs) =>
      exprs map (ex => ex.env_=(e.env))
      val cols1 = cols map (col => this walk col)
      val exprs1 = exprs map (ex => this walk ex)
      v.visit(e, cols1, exprs1)
      
   case e @ PosAccExpr (qualifier, selector) =>
      qualifier.env_=(e.env)
      val qualifier1 = this walk qualifier
      val selector1 = this walk selector
      v.visit(e, qualifier1, selector1)
      
   case e @ ListExpr (exprs) =>
      exprs map (ex => ex.env_=(e.env))
      val exprs1 = exprs map (ex => this walk ex)
      v.visit(e, exprs1)
      
   case e @ VarUseExpr (id) =>
      val id1 = this walk id
      v.visit(e, id1)
      
   case e @ LetExpr (bindings, body) =>
      bindings.env_=(e.env)
      val bindings1 = this walk bindings
      Set(e,body) map (ex => ex.env_=(bindings.env))
      val body1 = this walk body
      v.visit(e, bindings1, body1)
      
   case e @ IfExpr (cond, thencls, elsecls) =>
      Set(cond, thencls, elsecls) map (ex => ex.env_=(e.env))
      val cond1 = this walk cond
      val thencls1 = this walk thencls
      val elsecls1 = this walk elsecls
      v.visit(e, cond1, thencls1, elsecls1)
      
   case e @ ForExpr (bindings, wherecls, groupbycls, groupbywherecls, orderbycls, expr) =>
      bindings.env_=(e.env)
      val bindings1 = this walk bindings
      e.env_=(bindings.env)
      val wherecls1 = wherecls match {
        case Some(cls) =>
          cls.env_=(bindings.env)
          Some(this walk cls)
        case None => None
      }
      val groupbycls1 = groupbycls match {
        case Some(specs) => 
          specs.env_=(bindings.env)
          Some(this walk specs)
        case None => None
      }
      val groupbywherecls1 = groupbywherecls match {
        case Some(specs) => 
          specs.env_=(bindings.env)
          Some(this walk specs)
        case None => None
      }
      val orderbycls1 = orderbycls match {
        case Some(specs) => 
          specs.env_=(bindings.env)
          Some(this walk specs)
        case None => None
      }
      Set(expr) map (ex => ex.env_=(bindings.env))
      val expr1 = this walk expr
      v.visit(e, bindings1, wherecls1, groupbycls1, groupbywherecls1, orderbycls1, expr1)
      
   case e @ TableRefExpr (id, cspecs, kspecs, ospecs) =>
      Set(id, cspecs, kspecs) map (ex => ex.env_=(e.env))
      val id1 = this walk id
      val cspecs1 = this walk cspecs
      val kspecs1 = this walk kspecs
      val ospecs1 = ospecs match {
        case Some(specs) => 
          specs.env_=(e.env)
          Some(this walk specs)
        case None => None
      }
      v.visit(e, id1, cspecs1, kspecs1, ospecs1)
      
   case e @ NomAccExpr (expr, selector) =>
      Set(expr, selector) map (ex => ex.env_=(e.env))
      val expr1 = this walk expr
      val selector1 = this walk selector
      v.visit(e, expr1, selector1)
    
   case e @ FunAppExpr (id, args) =>
      Set(id, args) map (ex => ex.env_=(e.env))
      val id1 = this walk id
      val args1 = this walk args
      v.visit(e, id1, args1)
      
   case e @ ParenthesizedExpr (expr) =>
      expr.env_=(e.env)
      val expr1 = this walk expr
      v.visit(e, expr1)

  }
  
  def walk(e : FerryAttr) : T =  e match {
    
   case e @ NatLiteral (literal) =>
      v.visit(e, literal)
   
   case e @ VarIDDecl (id) =>
      v.visit(e, id)
   
   case e @ LetBindingClause (bindings) =>
      val exprs = bindings map (t => t._2)
      var envv: List[VarIDDecl] = e.env
      var bindings1: List[Tuple2[T,T]] = List()
        for (t <- bindings) {
          t._2.env_=(envv)
          envv = envv:::List(t._1) 
          bindings1 = bindings1 ::: List( Tuple2(this walk t._1, this walk t._2) )
          // updated
          v.visit(e, bindings1)
        }
      e.env_=(envv)
      v.visit(e, bindings1)   
   
   case e @ ForBindingClause (bindings) =>
      val exprs = bindings map (t => t._2)
      var envv: List[VarIDDecl] = e.env
      var bindings1: List[Tuple2[T,T]] = List()
        for (t <- bindings) {
          t._2.env_=(envv)
          envv = envv:::List(t._1) 
          bindings1 = bindings1 ::: List( Tuple2(this walk t._1, this walk t._2) )
          // updated
          v.visit(e, bindings1)
        }
      e.env_=(envv)
      v.visit(e, bindings1) 
      
   case e @ WhereClause (expr) =>
      expr.env_=(e.env)
      val expr1 = this walk expr
      v.visit(e, expr1)
      
   case e @ GroupByClause (exprs) =>
      exprs map (ex => ex.env_=(e.env))
      val exprs1 = exprs map (ex => this walk ex)
      v.visit(e, exprs1) 
      
   case e @ OrderSpec (expr, modifier) =>
      expr.env_=(e.env)
      val expr1 = this walk expr
      v.visit(e, expr1, modifier) 
      
   case e @ OrderByClause (specs) =>
      specs map (ex => ex.env_=(e.env))
      val specs1 = specs map (sp => this walk sp)
      v.visit(e, specs1) 
      
   case e @ TableID (id) =>
      v.visit(e, id)
      
   case e @ TableColID (id) =>
      v.visit(e, id) 
      
   case e @ TableColSpec (id, ctpt) =>
      val id1 = this walk id
      v.visit(e, id1, ctpt)
      
   case e @ TableColSpecs (specs) =>
      specs map (ex => ex.env_=(e.env))
      val specs1 = specs map (sp => this walk sp)
      v.visit(e, specs1) 
   
   case e @ TableKeySpec (cid) =>
      val cids1 = cid map (cid => this walk cid)
      v.visit(e, cids1)
      
   case e @ TableKeySpecs (specs) =>
      specs map (ex => ex.env_=(e.env))
      val specs1 = specs map (sp => this walk sp)
      v.visit(e, specs1) 
      
   case e @ TableOrderSpec (cid, modifier) =>
      val cid1 = this walk cid
      v.visit(e, cid1, modifier)
      
   case e @ TableOrderSpecs (specs) =>
      specs map (ex => ex.env_=(e.env))
      val specs1 = specs map (sp => this walk sp)
      v.visit(e, specs1) 
      
   case e @ FunID (id) =>
      v.visit(e, id)
      
   case e @ FunListArgs (exprs) =>
      exprs map (ex => ex.env_=(e.env))
      val exprs1 = exprs map (ex => this walk ex)
      v.visit(e, exprs1)
      
   case e @ FunCondArgs (id, lexpr, rexpr)=>
      rexpr.env_=(e.env)
      val rexpr1 = this walk rexpr
      // TODO: require redesigning of type derivation 
      if (rexpr.tpt.isInstanceOf[FList]) id.tpt_=(rexpr.tpt.asInstanceOf[FList].elemT)
      Set(e) map (ex => ex.env_=(e.env:::List(id)))
      lexpr.env_=(e.env)
      val id1 = this walk id
      val lexpr1 = this walk lexpr
      
      v.visit(e, id1, lexpr1, rexpr1)
      
   case _ => defaultRetVal
      
  }

}
  
}
  



// TODO make backup copies, please. 
