/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009 Anthony M Sloane, Macquarie University.
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

package kiama.example.lambda2

/**
 * Parser to AST.
 */
trait Parser extends kiama.parsing.CharPackratParsers {

    import AST._

    lazy val parse : MemoParser[Exp] =
        exp

    lazy val exp : MemoParser[Exp] =
        "\\" ~> idn ~ (":" ~> ttype) ~ ("." ~> exp) ^^ { case i ~ t ~ e => Lam (i, t, e) } |
        exp2

    lazy val exp2 : MemoParser[Exp] =
        exp2 ~ op ~ exp1 ^^ { case l ~ o ~ r => Opn (o, l, r) } |
        exp1

    lazy val exp1 : MemoParser[Exp] =
        exp1 ~ exp0 ^^ { case l ~ r => App (l, r) } |
        exp0

    lazy val exp0 : MemoParser[Exp] =
        number | idn ^^ Var | "(" ~> exp <~ ")"

    lazy val ttype : MemoParser[Type] =
        ttype0 ~ ("->" ~> ttype) ^^ { case l ~ r => FunType (l, r) } |
        ttype0

    lazy val ttype0 : MemoParser[Type] =
        "Int" ^^^ IntType |
        "(" ~> ttype <~ ")"

    lazy val op : MemoParser[Op] =
        "+" ^^^ AddOp |
        "-" ^^^ SubOp

    lazy val idn : MemoParser[Idn] =
        token (letter ~ (letterOrDigit*)) ^^ { case c ~ cs => c + cs.mkString }

    lazy val number : MemoParser[Num] =
        token (digit+) ^^ (l => Num (l.mkString.toInt))

}
