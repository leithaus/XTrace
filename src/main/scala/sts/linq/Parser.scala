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

import kiama.parsing.CharPackratParsers

/**
 * LINQ parser
 */
object Parser extends CharPackratParsers {
  
    import AbstractSyntax._
    
    def run (in : java.io.Reader) : QueryExp =
        parseAll (queryexp, in) match {
            case Success (r, _)     => r
            case f @ Failure (_, _) => error (f.toString)
        }    

    lazy val strsdotsep : Parser[List[String]] = IDENTIFIER + "."
    // lazy val strsdotsep : Parser[List[String]] = IDENTIFIER ~ ("." ~> IDENTIFIER) ^^ { case id1 ~ id2 => List(id1, id2) }

    lazy val queryexp : Parser[QueryExp] =
        fromclause ~ querybody ^^ { case from ~ qbody => QueryExp(from, qbody) }
    
    lazy val fromclause : Parser[FromClause] =
      fromclauseWithoutType | fromclauseWithType 
      
    lazy val fromclauseWithoutType : Parser[FromClause] =
      ("from" ~> IDENTIFIER) ~ ("in" ~> exp) ^^ { case variable ~ in => FromClause (List(), variable, in) }
    
    lazy val fromclauseWithType : Parser[FromClause] =
      ("from" ~> strsdotsep) ~ IDENTIFIER ~ ("in" ~> exp) ^^ { case tp ~ variable ~ in => FromClause (tp, variable, in) }
    
    lazy val querybody : Parser[QueryBody] = 
        (bodyclause*) ~ sg ~ (querycont?) ^^ { case qbclauses ~ selgby ~ qcont => QueryBody(qbclauses, selgby, qcont) }
    
    lazy val bodyclause : Parser[BodyClause] =
        fromclause | letclause | whereclause | joinintoclause | joinclause | orderbyclause 
    
    lazy val querycont : Parser[QueryCont] =
        "into" ~> IDENTIFIER ~ querybody ^^ { case variable ~ qbody => QueryCont (variable, qbody) }
    
    lazy val letclause : Parser[LetClause] =
        ("let" ~> IDENTIFIER) ~ ("=" ~> exp) ^^ { case lhs ~ rhs => LetClause(lhs, rhs) }
    lazy val whereclause : Parser[WhereClause] =
        "where" ~> exp ^^ { case booltest => WhereClause(booltest) }

    lazy val joinintoclause : Parser[JoinIntoClause] =
       joinclause ~ ("into" ~> IDENTIFIER ) ^^ { case jc ~ result => JoinIntoClause(jc, result) }
    
    lazy val joinclause : Parser[JoinClause] =
      joinclauseWithoutType  | joinclauseWithType  
       
    lazy val joinclauseWithType : Parser[JoinClause] = 
      "join" ~> (strsdotsep) ~ (IDENTIFIER <~ "in") ~ (exp <~ "on") ~ (exp <~ "equals") ~ exp ^^ 
      { case tp ~ innervar ~ innerexp ~ lhs ~ rhs => JoinClause(tp, innervar, innerexp, lhs, rhs) }
    
    lazy val joinclauseWithoutType : Parser[JoinClause] = 
      "join" ~> (IDENTIFIER <~ "in") ~ (exp <~ "on") ~ (exp <~ "equals") ~ exp ^^ 
      { case innervar ~ innerexp ~ lhs ~ rhs => JoinClause(List(), innervar, innerexp, lhs, rhs) }
    
    lazy val orderbyclause : Parser[OrderByClause] =
        "orderby" ~> (ordering + ",") ^^ { case orderings => OrderByClause(orderings) }
    
    lazy val ordering = exp ~ (direction?) ^^ { case ord ~ None => Ordering(ord, Direction.ascending); case ord ~ Some(dir) => Ordering(ord, dir) }
    
    lazy val direction = ("ascending" | "descending") ^^ { case "descending" => Direction.descending; case _ => Direction.ascending }
    
    lazy val sg : Parser[SG] = selectclause | groupbyclause
    
    lazy val selectclause : Parser[SelectClause] = "select" ~> exp ^^ { case selexp => SelectClause(selexp) }
    
    lazy val groupbyclause : Parser[GroupByClause] = "group" ~> exp ~ ("by" ~> exp) ^^ { case resexp ~ clusterexp => GroupByClause(resexp, clusterexp) }

    lazy val LINQKeyword : Parser[String] = 
        "from" | "where" | "select" | "group" | "into" | "orderby" | "join" | "let" | "in" | "on" | "equals" | "by" | "ascending" | "descending" 
    
    lazy val IDENTIFIER : Parser[String] =
        ( /* not (LINQKeyword) ~>*/ (token (letter ~ (letterOrDigit*))))  ^^ { case c ~ cs => c + cs.mkString }
    
    lazy val comment =
        '/' ~> '/' ~> ((not (endofline) ~> letterOrDigit)*) <~ endofline // TODO instead of letterOrDigit should by any
    lazy val endofline =
        '\r' ~ '\n' | '\r' | '\n'    
    override lazy val layout =
        ((whitespace | comment)*) ^^^ List()

    
    lazy val eqoper : Parser[EqOper.Value] = ("==" ^^^ EqOper.eq) | ("!=" ^^^ EqOper.ne) 
    lazy val reloper : Parser[RelOper.Value] = ("<=" ^^^ RelOper.le) | ("<" ^^^ RelOper.lt) | (">=" ^^^ RelOper.ge) | (">" ^^^ RelOper.gt) 
    lazy val addoper : Parser[AddOper.Value] = ("+" ^^^ AddOper.plus) | ("-" ^^^ AddOper.minus)
    lazy val multoper : Parser[MultOper.Value] = ("*" ^^^ MultOper.mult) | ("/" ^^^ MultOper.div) | ("%" ^^^ MultOper.modulo) 
    lazy val unaryoper : Parser[UnaryOper.Value] = ("!" ^^^ UnaryOper.neg) | ("-" ^^^ UnaryOper.minus) 

    lazy val exp =
        queryexp | orexp | andexp | eqexp | relexp | addexp | multexp | stmtexp | primexp   

    lazy val orexp : MemoParser[Exp] =
        orexp ~ ("||" ~> andexp) ^^ { 
          case (oe @ OrExp(rands)) ~ ae => OrExp(rands ::: List(ae))
          case oe ~ ae => OrExp(List(oe, ae))
        } | 
        andexp
    
    lazy val andexp : MemoParser[Exp] =
        andexp  ~ ("&&" ~> eqexp) ^^ { 
          case (ae @ AndExp(rands)) ~ ee => AndExp(rands ::: List(ee)) 
          case ae ~ ee => AndExp(List(ae, ee))
        } | 
        eqexp
    
    lazy val eqexp : MemoParser[Exp] =
        eqexp ~ eqoper ~ relexp ^^ { case lhs ~ oper ~ rhs => EqExp(lhs, rhs, oper) } | 
        relexp
        
    lazy val relexp : MemoParser[Exp] =
        relexp ~ reloper ~ addexp ^^ { case lhs ~ oper ~ rhs => RelExp(lhs, rhs, oper) } | 
        addexp
    
    lazy val addexp : MemoParser[Exp] =
        addexp ~ addoper ~ multexp ^^ { case lhs ~ oper ~ rhs => AddExp(lhs, rhs, oper) } |
        multexp

    lazy val multexp : MemoParser[Exp] =
        multexp ~ multoper ~ unaryexp ^^ { case lhs ~ oper ~ rhs => MultExp(lhs, rhs, oper) } | 
        unaryexp
    
    lazy val unaryexp : MemoParser[Exp] =
        unaryoper ~ stmtexp ^^ { 
          case oper ~ stmt => UnaryExp(oper, stmt) 
        } | 
        stmtexp
    
    lazy val stmtexp : MemoParser[Exp] =
        stmtexp ~ ("." ~> primexp) ^^ { 
          case (s @ Stmt(dotseparated)) ~ p => 
            Stmt(dotseparated ::: List(p));
          case s ~ p => 
            Stmt(List(s, p))
        } |
        primexp
    
    lazy val primexp : MemoParser[Exp] = newexp | methodcall | nestedexp | valueexp 
    
    lazy val nestedexp : MemoParser[NestedExp] = ("(" ~> exp) <~ ")" ^^ { case e => NestedExp(e) } 
    
    lazy val valueexp : Parser[ValueExp] = intlit | doublelit | stringlit | boollit | useexp 
    lazy val intlit : Parser[IntLiteral] = INTLITERAL ^^ { case i => IntLiteral(i) }
    lazy val doublelit : Parser[DoubleLiteral] = DOUBLELITERAL ^^ { case str => DoubleLiteral(java.lang.Double.parseDouble(str)) }
    lazy val stringlit : Parser[StringLiteral] = "\"" ~> IDENTIFIER <~ "\"" ^^ { case str => StringLiteral(str) }
    lazy val boollit : Parser[BoolLiteral] = ("true" ^^ { case _ => BoolLiteral(true) }) | ("false" ^^ { case _ => BoolLiteral(false) })
    lazy val useexp : Parser[UseExp] = IDENTIFIER ^^ { case ident => UseExp(ident) } 
    
    lazy val INTLITERAL : Parser[Int] = token (digit+) ^^ (l => l.mkString.toInt)  
    
    lazy val DOUBLELITERAL : Parser[String] = // TODO account for scientific notation  
      (INTLITERAL <~ ".") ~ INTLITERAL ^^ { case intpart ~ decpart => intpart + "." + decpart  }

    lazy val methodcall : Parser[Exp] = IDENTIFIER ~ (castexp?) ~ ("(" ~> (exp * ",") <~ ")") ^^ {
      case methodname ~ optioncast ~ args => MethodCall(methodname, optioncast, args) }
    
    lazy val castexp : Parser[List[String]] = "<" ~> (strsdotsep <~ ">") ^^ { case ds => ds }
    
    lazy val newexp : Parser[NewExp] = newexpwithinit | newexptraditional
    
    lazy val newexpwithinit : Parser[NewExpWithInit] = ("new" ~> (strsdotsep?)) ~ ("{" ~> ((newfieldinit * ",") <~ "}")) ^^ 
      { case tp ~ fieldinits => NewExpWithInit(tp, (List.unzip(fieldinits))._1, (List.unzip(fieldinits))._2) }
    
    lazy val newexptraditional : Parser[NewExpTraditional] = ("new" ~> strsdotsep) ~ ("(" ~> ((exp * ",") <~ ")")) ^^ 
      { case tp ~ args => NewExpTraditional(tp, args) }
    
    lazy val newfieldinit : Parser[(Option[String], Exp)] = ((IDENTIFIER <~ "=")?) ~ exp ^^ { case to ~ from => (to, from)  } 
    
}

