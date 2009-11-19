/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2009 Anthony M Sloane, Macquarie University.
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
 * along with Kiama.  (See files COPYING and COPYING.LESSER.)  If not, see
 * <http://www.gnu.org/licenses/>.
 */

/**
 * This file is derived from a JastAdd implementation of PicoJava, created
 * in the Department of Computer Science at Lund University.  See the
 * following web site for details:
 *
 * http://jastadd.cs.lth.se/examples/PicoJava/index.shtml
 */

package kiama.example.picojava

import kiama.parsing.CharPackratParsers

/**
 * PicoJava parser
 */
object Parser extends CharPackratParsers {

    import AbstractSyntax._

    def run (in : java.io.Reader) : Program =
        parseAll (program, in) match {
            case Success (r, _)     => r
            case f @ Failure (_, _) => error (f.toString)
        }

    lazy val program : Parser[Program] =
        block ^^ Program

    lazy val block : Parser[Block] =
        "{" ~> (block_stmt*) <~ "}" ^^ { case bs => Block (bs) }
    lazy val block_stmt =
        class_decl | var_decl | stmt

    lazy val class_decl =
        "class" ~> IDENTIFIER ~ (xtends?) ~ block ^^ { case i ~ e ~ b => ClassDecl (i, e, b) }
    lazy val xtends =
        "extends" ~> IDENTIFIER ^^ Use
    lazy val var_decl =
        name ~ IDENTIFIER <~ ";" ^^ { case n ~ i => VarDecl (i, n) }

    lazy val stmt : Parser[Stmt] =
        assign_stmt | while_stmt
    lazy val assign_stmt =
        name ~ ("=" ~> exp <~ ";") ^^ { case n ~ e => AssignStmt (n, e) }
    lazy val while_stmt =
        ("while" ~> "(" ~> exp <~ ")") ~ stmt ^^ { case e ~ s => WhileStmt (e, s) }

    lazy val exp =
        name | boolean_literal

    lazy val name : MemoParser[Access] =
        name ~ ("." ~> IDENTIFIER) ^^ { case n ~ i => Dot (n, Use (i)) } |
        IDENTIFIER ^^ Use

    lazy val boolean_literal =
        ("true" | "false") ^^ BooleanLiteral

    lazy val IDENTIFIER : MemoParser[String] =
        token (letter ~ (letterOrDigit*)) ^^ { case c ~ cs => c + cs.mkString }

    lazy val comment =
        '/' ~> '/' ~> ((not (endofline) ~> any)*) <~ endofline
    lazy val endofline =
        '\r' ~ '\n' | '\r' | '\n'
    override lazy val layout =
        ((whitespace | comment)*) ^^^ List()

}
