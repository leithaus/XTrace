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

package kiama.example.lambda

import kiama.rewriting.Rewriter
import kiama.util.GeneratingREPL
import kiama.util.ParsingREPL

/**
 * A simple lambda calculus.
 */
object AST {

    /**
     * Identifiers are represented as strings.
     */
    type Idn = String

    /**
     * Expressions.
     */
    abstract class Exp extends Product

    /**
     * Numeric expressions.
     */
    case class Num (i : Int) extends Exp {
        override def toString = i.toString
    }

    /**
     * Variable expressions.
     */
    case class Var (x : Idn) extends Exp {
        override def toString = x
    }

    /**
     * Lambda expressions binding x within e.
     */
    case class Lam (x : Idn, e : Exp) extends Exp {
        override def toString = "(\\" + x + "." + e + ")"
    }

    /**
     * Application of l to r.
     */
    case class App (l : Exp, r : Exp) extends Exp {
        override def toString = "(" + l + " " + r + ")"
    }

    /**
     * Substitution of n for x within m.
     */
    case class Sub (m : Exp, x : Idn, n : Exp) extends Exp

}

/**
 * Parser to AST.
 */
trait Parser extends kiama.parsing.CharPackratParsers {

    import AST._

    lazy val parse : Parser[Exp] =
        phrase (exp)

    lazy val exp : MemoParser[Exp] =
        exp ~ factor ^^ { case l ~ r => App (l, r) } |
        ("\\" ~> idn) ~ ("." ~> exp) ^^ { case i ~ b => Lam (i, b) } |
        factor |
        failure ("expression expected")

    lazy val factor : MemoParser[Exp] =
        integer | variable | "(" ~> exp <~ ")"

    lazy val integer : Parser[Num] =
        token (digit+) ^^ (l => Num (l.mkString.toInt))

    lazy val variable : Parser[Var] =
        idn ^^ Var

    lazy val idn : Parser[String] =
        token (letter ~ (letterOrDigit*)) ^^ { case c ~ cs => c + cs.mkString }

}

/**
 * ScalaCheck generators for programs in the lambda language.
 */
trait Generator {

    import org.scalacheck._
    import AST._

    val genNum = for (i <- Gen.choose (1, 100)) yield Num (i)
    val genIdn : Gen[String] = for (s <- Gen.identifier) yield (s.take (5))
    val genVar = for (v <- genIdn) yield Var (v)

    implicit def arbVar : Arbitrary[Var] = Arbitrary (genVar)

    val genLeafExp = Gen.oneOf (genNum, genVar)

    def genLamExp (sz : Int) =
        for { i <- genIdn; b <- genExp (sz/2) } yield Lam (i, b)

    def genAppExp (sz : Int) =
        for { l <- genExp (sz/2); r <- genExp (sz/2) } yield App (l, r)

    def genExp (sz : Int) : Gen[Exp] =
        if (sz <= 0)
            genLeafExp
        else
            Gen.frequency ((1, genLeafExp), (1, genLamExp (sz)), (3, genAppExp (sz)))

    implicit def arbExp : Arbitrary[Exp] =
        Arbitrary { Gen.sized (sz => genExp (sz)) }

}

/**
 * Basis for tests using the lambda calculus language.  Includes access to the
 * parser, generator and evaluator.
 */
trait TestBase extends Parser with Generator with Evaluator

/**
 * Lambda calculus evaluator following Rose's \xgc, ie with explicit
 * substitutions and garbage collection.  See "Explicit Substitution
 * - Tutorial and Survey, Kristoffer H. Rose, BRICS LS-96-3, October
 * 1996.
 */
trait Evaluator extends Rewriter {

    import AST._

    /**
     * Free variables
     */
    def fv (t : Exp) : Set[Idn] = {
        t match {
            case Num (_)       => Set ()
            case Var (x)       => Set (x)
            case Lam (x, e)    => fv (e) - x
            case App (m, n)    => fv (m) ++ fv (n)
            case Sub (m, x, n) => (fv (m) - x) ++ fv (n)
        }
    }

    /**
     * \xgc-reduction
     */
    val xgc_reduction =
        rule {
            // Substitution generation
            case App (Lam (x, e1), e2)           => Sub (e1, x, e2)

            // Explicit substitution
            case Sub (Var (x), y, n) if (x == y) => n
            case Sub (Var (x), _, _)             => Var (x)
            case Sub (Lam (x, m), y, n)          => Lam (x, Sub (m, y, n))
            case Sub (App (m1, m2), y, n)        => App (Sub (m1, y, n), Sub (m2, y, n))

            // Garbage collection
            case Sub (m, x, n) if ! (fv (m) contains (x))
                                                 => m
        }

    /**
     * Normal-order reduction
     */
    val normal = outermost (xgc_reduction)

}

/**
 * A read-eval-print loop for evaluation of lambda calculus expressions.
 */
object Lambda extends ParsingREPL[AST.Exp] with Parser with Evaluator {

    import AST._

    override def setup { println ("Enter lambda calculus expressions for evaluation.") }
    override def prompt = "lambda> "

    def process (e : Exp) {
        normal (e) match {
            case Some (r) => println (r)
            case None     => println ("reduction failed")
        }
    }

}

/**
 * A read-eval-print loop for generating random expressions.
 */
object LambdaGen extends GeneratingREPL[AST.Exp] with Generator {
    def generator = arbExp
}
