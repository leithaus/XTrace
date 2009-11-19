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

package kiama.example.imperative

import kiama.util.GeneratingREPL
import kiama.util.ParsingREPL

/**
 * A simple imperative language abstract syntax designed for testing.
 */
object AST {

    /**
     * Identifiers are represented as strings.
     */
    type Idn = String

    /**
     * Simple interface for pretty-printing capabilities.
     */
    trait PrettyPrintable {

        /**
         * Pretty-print the object at the end of the given string builder.
         */
        def pretty (o : StringBuilder)

    }

    /**
     * Expressions.
     */
    abstract class Exp extends PrettyPrintable with Product {

        /**
         * The numeric value of the expression.
         */
        def value : Double

        /**
         * The set of all variable references in the expression.
         */
        def vars : Set[Idn] = Set ()

        /**
         * The number of divisions by the constant zero in the expression.
         */
        def divsbyzero : Int = 0

        /**
         * The depth of the expression, i.e., the number of levels from the
         * root to the leaf values.
         */
        def depth : Int = 0

        /**
         * The number of additions of integer constants in the expression.
         */
        def intadds : Int = 0
    }

    /**
     * Numeric expressions.
     */
    case class Num (d : Double) extends Exp {
        override def value = d
        override def depth = 2
        def pretty (o : StringBuilder) = o.append (d)
    }

    /**
     * Variable expressions.
     */
    case class Var (s : Idn) extends Exp {
        // Hack to make tests more interesting
        override def value = 3
        override def vars = Set (s)
        override def depth = 2
        override def toString = "Var(\"" + s + "\")"
        def pretty (o : StringBuilder) = o.append (s)
    }

    /**
     * Unary negation expressions.
     */
    case class Neg (e : Exp) extends Exp {
        override def value = - e.value
        override def vars = e.vars
        override def divsbyzero = e.divsbyzero
        override def depth = 1 + e.depth
        override def intadds = e.intadds
        def pretty (o : StringBuilder) = {
            o.append ("(-"); e.pretty (o); o.append (')')
        }
    }

    /**
     * Binary expressions.
     */
    abstract class Binary (l : Exp, r : Exp) extends Exp {
        override def vars = l.vars ++ r.vars
        override def divsbyzero = l.divsbyzero + r.divsbyzero
        override def depth = 1 + (l.depth).max (r.depth)
        override def intadds = l.intadds + r.intadds
    }

    /**
     * Addition expressions.
     */
    case class Add (l : Exp, r : Exp) extends Binary (l, r) {
        override def value = l.value + r.value
        override def intadds =
            (l, r) match {
                case (Num (_), Num (_)) => 1
                case _                  => super.intadds
            }
        def pretty (o : StringBuilder) = {
            o.append ('('); l.pretty (o); o.append (" + "); r.pretty (o); o.append (')')
        }
    }

    /**
     * Subtraction expressions.
     */
    case class Sub (l : Exp, r : Exp) extends Binary (l, r) {
        override def value = l.value - r.value
        def pretty (o : StringBuilder) = {
            o.append ('('); l.pretty (o); o.append (" - "); r.pretty (o); o.append (')')
        }
    }

    /**
     * Multiplication expressions.
     */
    case class Mul (l : Exp, r : Exp) extends Binary (l, r) {
        override def value = l.value * r.value
        def pretty (o : StringBuilder) = {
            o.append ('('); l.pretty (o); o.append (" * "); r.pretty (o); o.append (')')
        }
    }

    /**
     * Division expressions.
     */
    case class Div (l : Exp, r : Exp) extends Binary (l, r) {
        // Hack: no errors, so return zero for divide by zero
        override def value = if (r.value == 0) 0 else l.value / r.value
        override def divsbyzero =
            l.divsbyzero + (r match {
                                case Num (0) => 1
                                case _       => r.divsbyzero
                            })
        def pretty (o : StringBuilder) = {
            o.append ('('); l.pretty (o); o.append (" / "); r.pretty (o); o.append (')')
        }
    }

    /**
     * Statements.
     */
    abstract class Stmt extends PrettyPrintable with Product {

        /**
         * The set of all variable references in the statement.
         */
        def vars : Set[Idn] = Set ()

    }

    /**
     * Empty statements.
     */
    case class Null () extends Stmt {
        def pretty (o : StringBuilder) = o.append (";\n")
    }

    /**
     * Statement sequences.
     */
    case class Seqn (ss : Seq[Stmt]) extends Stmt {
        override def vars = Set (ss flatMap (_ vars) : _*)
        def pretty (o : StringBuilder) = {
            o.append ("{\n"); ss.foreach (_.pretty (o)); o.append ("}\n")
        }
    }

    /**
     * Assignment statements.
     */
    case class Asgn (s : Idn, e : Exp) extends Stmt {
        override def vars = Set (s)
        override def toString = "Asgn(\"" + s + "\"," + e + ")"
        def pretty (o : StringBuilder) = {
            o.append (s); o.append (" = "); e.pretty (o); o.append (";\n")
        }
    }

    /**
     * While loops.
     */
    case class While (e : Exp, b : Stmt) extends Stmt {
        override def vars = e.vars ++ b.vars
        def pretty (o : StringBuilder) = {
            o.append ("while ("); e.pretty (o); o.append (")\n");
            b.pretty (o);
        }
    }

}

/**
 * AST pretty-printing.
 */
trait PrettyPrinter {

    import AST._

    /**
     * Return a pretty-printed version of t.
     */
    def pretty[T <: PrettyPrintable] (t : T) : String = {
        val buffer = new StringBuilder
        t.pretty (buffer)
        buffer.toString
    }

}

/**
 * Parser to AST.
 */
trait Parser extends kiama.parsing.CharPackratParsers {

    import AST._

    lazy val parse : Parser[Stmt] =
        phrase (stmt)

    lazy val stmt : Parser[Stmt] =
        ";" ^^^ Null () | sequence | asgnStmt | whileStmt

    lazy val asgnStmt : Parser[Asgn] =
        idn ~ ("=" ~> exp) <~ ";" ^^ { case s ~ e => Asgn (s, e) }

    lazy val whileStmt : Parser[While] =
        ("while" ~> "(" ~> exp <~ ")") ~ stmt ^^ { case e ~ b => While (e, b) }

    lazy val sequence : Parser[Seqn] =
        "{" ~> (stmt*) <~ "}" ^^ Seqn

    lazy val exp : MemoParser[Exp] =
        exp ~ ("+" ~> term) ^^ { case l ~ r => Add (l, r) } |
        exp ~ ("-" ~> term) ^^ { case l ~ r => Sub (l, r) } |
        term

    lazy val term : MemoParser[Exp] =
        term ~ ("*" ~> factor) ^^ { case l ~ r => Mul (l, r) } |
        term ~ ("/" ~> factor) ^^ { case l ~ r => Div (l, r) } |
        factor

    lazy val factor : MemoParser[Exp] =
        double | integer | variable | "-" ~> exp | "(" ~> exp <~ ")"

    lazy val double : Parser[Num] =
        token ((digit+) ~ ("." ~> (digit+))) ^^ { case l ~ r => Num ((l.mkString + "." + r.mkString).toDouble) }

    lazy val integer : Parser[Num] =
        token (digit+) ^^ (l => Num (l.mkString.toInt))

    lazy val variable : Parser[Var] =
        idn ^^ Var

    lazy val idn : Parser[String] =
        !keyword ~> token (letter ~ (letterOrDigit*)) ^^ { case c ~ cs => c + cs.mkString }

    lazy val keyword : Parser[String] =
        "while"

}

/**
 * ScalaCheck generators for programs in the imperative language.
 */
trait Generator {

    import org.scalacheck._
    import AST._

    val genInteger = for (i <- Gen.choose (1, 100)) yield Num (i)
    val genDouble = for (i <- Gen.choose (1.0, 1000000.0)) yield Num (i)
    val genNum = Gen.frequency ((3, genInteger), (1, genDouble))

    implicit def arbNum : Arbitrary[Num] =
        Arbitrary (genNum)

    val genIdn : Gen[String] = for (s <- Gen.identifier) yield (s.take (5))
    val genVar = for (v <- genIdn) yield Var (v)

    val genLeafExp = Gen.oneOf (genNum, genVar)

    def genNeg (sz : Int) =
        for { e <- genExp (sz/2) } yield Neg (e)

    def genAdd (sz : Int) =
        for { l <- genExp (sz/2); r <- genExp (sz/2) } yield Add (l, r)

    def genSub (sz : Int) =
        for { l <- genExp (sz/2); r <- genExp (sz/2) } yield Sub (l, r)

    def genMul (sz : Int) =
        for { l <- genExp (sz/2); r <- genExp (sz/2) } yield Mul (l, r)

    def genDiv (sz : Int) =
        for { l <- genExp (sz/2); r <- genExp (sz/2) } yield Div (l, r)

    def genInternalExp (sz : Int) =
        Gen.oneOf (genAdd (sz), genSub (sz), genMul (sz), genDiv (sz))

    def genExp (sz : Int) : Gen[Exp] =
        if (sz <= 0)
            genLeafExp
        else
            Gen.frequency ((1, genLeafExp), (3, genInternalExp (sz)))

    implicit def arbExp : Arbitrary[Exp] =
        Arbitrary { Gen.sized (sz => genExp (sz)) }

    val genLeafStmt = Gen.value (Null ())

    def genSeqn (sz : Int) =
        for { len <- Gen.choose (1,sz)
              ss <- Gen.containerOfN[List,Stmt] (len, genStmt (sz / len)) }
            yield Seqn (ss)

    implicit def arbSeqn : Arbitrary[Seqn] =
        Arbitrary { Gen.sized (sz => genSeqn (sz)) }

    def genAsgn (sz : Int) =
        for { i <- genIdn; e <- genExp (sz-1) } yield Asgn (i, e)

    implicit def arbAsgn : Arbitrary[Asgn] =
        Arbitrary { Gen.sized (sz => genAsgn (sz)) }

    def genWhile (sz : Int) =
        for { e <- genExp (sz/3); b <- genStmt (sz - 1) } yield While (e, b)

    implicit def arbWhile : Arbitrary[While] =
        Arbitrary { Gen.sized (sz => genWhile (sz)) }

    def genInternalStmt (sz : Int) : Gen[Stmt] =
        Gen.frequency ((1, genSeqn (sz)), (5, genAsgn (sz)), (3, genWhile (sz)))

    def genStmt (sz : Int) =
        if (sz <= 0)
            genLeafStmt
        else
            Gen.frequency ((1, genLeafStmt), (9, genInternalStmt (sz)))

    implicit def arbStmt : Arbitrary[Stmt] =
        Arbitrary { Gen.sized (sz => genStmt (sz)) }

}

/**
 * Basis for tests using the imperative language.  Includes support for generating
 * random AST instances plus convenient access to the parser and pretty-printer.
 */
trait TestBase extends Generator with Parser with PrettyPrinter

/**
 * A read-eval-print loop for parsing imperative programs and printing thei
 * abstract synax trees.
 */
object Imperative extends ParsingREPL[AST.Stmt] with Parser {

    override def setup { println ("Enter imperative language programs for parsing.") }
    override def prompt = "imperative> "

    def process (s : AST.Stmt) {
        println (s)
    }

}

/**
 * A read-eval-print loop for generating random imperative statements.
 */
object ImperativeGen extends GeneratingREPL[AST.Stmt] with Generator with PrettyPrinter {

    def generator = arbStmt

    override def process (s : AST.Stmt) {
        println (s)
        println (pretty (s))
    }

}
