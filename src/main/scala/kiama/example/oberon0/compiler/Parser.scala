/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009 Anthony M Sloane, Macquarie University.
 *
 * Contributed by Ben Mockler.
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

package kiama.example.oberon0.compiler

import kiama.parsing.CharPackratParsers

/**
 * Parse Oberon-0 to an abstract syntax tree.
 */
trait Parser extends kiama.parsing.CharPackratParsers {

    import AST._

    lazy val parse  : Parser[ModuleDecl] =
        phrase (moduledecl)

    // Declarations

    lazy val moduledecl : Parser[ModuleDecl] =
        "MODULE" ~> (ident <~ ";") ~ declarations ~
            (("BEGIN" ~> statementSequence)?) ~ ("END" ~> ident) <~ "." ^^
                { case id1 ~ decs ~ opstseq ~ id2 =>
                     ModuleDecl (id1.name, decs, optionalListToList (opstseq), id2.name, ModuleType()) }

    lazy val declarations : Parser[List[Declaration]] =
        (constdecls?) ~ (typedecls?) ~ (vardecls?) ~ procdecls  ^^
             { case c ~ t ~ v ~ p =>
                  optionalListToList (c) ::: optionalListToList (t) :::
                  optionalListToList (v) ::: p }

    lazy val constdecl : Parser[ConstDecl] =
        (ident <~ "=") ~ expression <~ ";" ^^ { case id ~ ex => ConstDecl (id.name, ex) }

    lazy val constdecls : Parser[List[ConstDecl]] =
        "CONST" ~> (constdecl*)

    lazy val typedecl : Parser[TypeDecl] =
        (ident <~ "=") ~ type1 <~ ";" ^^ {case id ~ tp => TypeDecl (id.name, tp) }

    lazy val typedecls : Parser[List[TypeDecl]] =
        "TYPE" ~> (typedecl*)

    lazy val vardeclspertype : Parser[List[VarDecl]] =
        (identList <~ ":") ~ type1 ^^
            { case lst ~ tp => lst.map (id => VarDecl (id.name, tp)) }

    lazy val vardecls : Parser[List[VarDecl]] =
        "VAR" ~> ((vardeclspertype <~ ";")*) ^^ (lst => lst.flatten)

    lazy val fpSection : Parser[List[Declaration]] =
        (("VAR"?) ~ identList <~ ":") ~ type1 ^^
           { case Some (_) ~ lst ~ tp => lst.map (id => RefVarDecl (id.name, tp))
             case None     ~ lst ~ tp => lst.map (id => VarDecl (id.name, tp)) }

    lazy val formalParameters : Parser[List[Declaration]] =
        "(" ~> repsep(fpSection, ";") <~ ")" ^^ (lst => lst.flatten)

    lazy val procdecls : Parser[List[ProcDecl]] =
        (procdecl <~ ";")*

    lazy val procdecl : Parser[ProcDecl] =
        ("PROCEDURE" ~> ident) ~ (formalParameters?) ~ (";" ~> declarations) ~
            (("BEGIN" ~> statementSequence)?) ~ ("END" ~> ident) ^^
                { case id1 ~ opfps ~ decs ~ opstseq ~ id2 =>
                     ProcDecl (id1.name, optionalListToList (opfps), decs,
                               optionalListToList (opstseq), id2.name, ProcType(optionalListToList (opfps))) }

    // Types

    lazy val type1 : Parser[Type] =
        "INTEGER" ^^ { case _ => IntegerType } |
        ident ^^ NamedType |
        arrayType |
        recordType

    lazy val identList : Parser[List[Ident]] =
        repsep (ident, ",")

    lazy val arrayType : Parser[ArrayType] =
        (("ARRAY" ~> expression) <~ "OF") ~ type1 ^^
            { case e ~ tp => ArrayType (e, tp) }

    lazy val fieldList : Parser[List[FieldDecl]] =
        (identList <~ ":") ~ type1 ^^
            { case lst ~ tp => lst.map (id => FieldDecl (id.name, tp)) }

    lazy val recordType : Parser[RecordType] =
        "RECORD" ~> repsep (fieldList, ";") <~ "END" ^^
            (lst => RecordType (lst.flatten))

    // Statements

    lazy val statementSequence : Parser[List[Statement]] =
        repsep (statement, ";")

    lazy val statement : Parser[Statement] =
        assignment |
        procedureCall |
        ifStatement |
        whileStatement

    lazy val assignment : Parser[Assignment] =
        (desig <~ ":=") ~ expression ^^ { case d ~ e => Assignment (d, e) }

    lazy val actualParameters : Parser[List[Exp]] =
        "(" ~> repsep (expression, ",") <~ ")"

    lazy val procedureCall : Parser[Statement] =
        desig ~ actualParameters ^^ { case d ~ aps => ProcedureCall (d, aps) } |
        desig ^^ (d => ProcedureCall (d, Nil))

    lazy val ifStatement : Parser[IfStatement] =
        ("IF" ~> expression) ~ ("THEN" ~> statementSequence) <~ "END" ^^
            { case con ~ thnss => IfStatement (con, thnss, Nil) } |
        ("IF" ~> expression) ~ ("THEN" ~> statementSequence) ~ ifTail ^^
            { case con ~ thnss ~ els => IfStatement (con, thnss, els) }

    lazy val ifTail : Parser[List[Statement]] =
        ("ELSIF" ~> expression) ~ ("THEN" ~> statementSequence) ~ ifTail ^^
            { case con ~ thnss ~ els => List (IfStatement (con, thnss, els)) } |
        ("ELSIF" ~> expression) ~ ("THEN" ~> statementSequence) <~ "END" ^^
            { case con ~ thnss => List (IfStatement (con, thnss, Nil)) } |
        ("ELSE" ~> statementSequence) <~ "END"

    lazy val whileStatement : Parser[WhileStatement] =
        (("WHILE" ~> expression) <~ "DO") ~ statementSequence <~ "END" ^^
            { case ex ~ ss => WhileStatement (ex, ss) }

    // Expressions

    lazy val expression : MemoParser[Exp] =
        (simpleExpression <~ "=") ~ simpleExpression ^^ { case se1 ~ se2 => Equal (se1, se2) } |
        (simpleExpression <~ "#") ~ simpleExpression ^^ { case se1 ~ se2 => NotEqual (se1, se2) } |
        (simpleExpression <~ "<") ~ simpleExpression ^^ { case se1 ~ se2 => LessThan (se1, se2) } |
        (simpleExpression <~ "<=") ~ simpleExpression ^^ { case se1 ~ se2 => LessThanOrEqual (se1, se2) } |
        (simpleExpression <~ ">") ~ simpleExpression ^^ { case se1 ~ se2 => GreaterThan (se1, se2) } |
        (simpleExpression <~ ">=") ~ simpleExpression ^^ { case se1 ~ se2 => GreaterThanOrEqual (se1, se2) } |
        simpleExpression

    lazy val simpleExpression : MemoParser[Exp] =
        (simpleExpression <~ "+") ~ term ^^ {case se ~ t => Plus (se, t) } |
        (simpleExpression <~ "-") ~ term ^^ {case se ~ t => Minus (se, t) } |
        (simpleExpression <~ "OR") ~ term ^^ {case se ~ t => Or (se, t) } |
        term

    lazy val term : MemoParser[Exp] =
        (term <~ "*") ~ factor ^^ { case t ~ f => Mult (t, f) } |
        (term <~ "DIV") ~ factor ^^ { case t ~ f => Div (t, f) } |
        (term <~ "MOD") ~ factor ^^ { case t ~ f => Mod (t, f) } |
        (term <~ "&") ~ factor ^^ { case t ~ f => And (t, f) } |
        factor

    lazy val factor : MemoParser[Exp] =
        desig |
        number |
        "(" ~> expression <~ ")" |
        "~" ~> factor ^^ Not |
        "+" ~> factor ^^ Pos |
        "-" ~> factor ^^ Neg

    lazy val desig : MemoParser[Desig] =
        (desig <~ "[") ~ (expression <~ "]") ^^ { case d ~ e => ArrayDesig (d, e) } |
        (desig <~ ".") ~ ident ^^ { case d ~ id => FieldDesig (d, id) } |
        ident

    lazy val number : Parser[Literal] =
        integer

    lazy val keyword : Parser[String] =
        "ARRAY" | "BEGIN" | "CONST" | "ELSIF" | "ELSE" | "END" | "IF" |
        "MODULE" | "OF" | "PROCEDURE" | "RECORD" | "THEN" | "TYPE" | "VAR" |
        "WHILE"

    lazy val ident : MemoParser[Ident] =
        !keyword ~> token (letter ~ (letterOrDigit*)) ^^
        { case c ~ cs => Ident (c + cs.mkString) }

    lazy val integer : Parser[IntegerLiteral] =
        token (digit+) ^^ (l => IntegerLiteral(l.mkString.toInt))

    lazy val comment =
        "(*" ~> ((not ("*)") ~> any)*) <~ "*)"

    override lazy val layout =
        ((whitespace | comment)*) ^^^ List()

    /**
     * Convert an option list into either the list (if present) or Nil if None.
     */
    def optionalListToList[T] (op: Option[List[T]]) : List[T] =
        op.getOrElse (Nil)
}
