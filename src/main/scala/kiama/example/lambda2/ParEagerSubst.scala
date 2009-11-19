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
 * Eager evaluation of lambda calculus with parallel term-level substitution
 * and arithmetic operations.
 */
trait ParEagerSubst extends ReduceSubst {

    import AST._

    /**
     * Eagerly evaluate within the expression then try to reduce the
     * expression itself, repeating until no change.  Would be much
     * nicer if we had congruences!
     */
    override lazy val evals : Strategy =
        attempt (traverse) <* {
            def evaly (exp : AST.Exp) : AST.Exp =
                rewrite (y) (exp)
            lazy val y : Strategy =
                attempt (rule {
                             case App (e1, e2)     => App (evaly (e1), evaly (e2))
                             case Opn (op, e1, e2) => Opn (op, evaly (e1), evaly (e2))
                         }) <* e
            lazy val e = attempt (lambda <* y)
            e
        }

    /**
     * Recursively try to eagerly evaluate expressions in applications,
     * substitutions and operations, but not within lambdas.
     */
    lazy val traverse : Strategy =
        rule {

            // In an application we need to traverse to both sub-expressions.
            case App (e1, e2)       => App (eval (e1), eval (e2))

            // In a parallel substitution, traverse to all bound expressions.
            case Letp (ds, e)       =>
                Letp (ds map {
                          case Bind (x, t, e1) => Bind (x, t, eval (e1))
                      }, e)

            // In an operation we need to traverse to both sub-expressions.
            case Opn (op, e1, e2)   => Opn (op, eval (e1), eval (e2))

        }

    /**
     * Reusable strategy for reduction with explicit term-level substitution.
     */
    override lazy val lambda =
        beta + arithop + subsNum + subsVar + subsApp + subsLam + subsOpn +
        letLetOne + letLet

    /**
     * Beta reduction via term-level substitution.
     */
    override lazy val beta =
        rule {
            case App (Lam (x, t, e1), e2) =>
                val y = freshvar ()
                Letp (List (Bind (y, t, e2)),
                      Letp (List (Bind (x, t, Var (y))), e1))
        }

    /**
     * Substitution in numeric terms.
     */
    override lazy val subsNum =
        rule {
            case Letp (_, e : Num) => e
        }

    /**
     * Substitution in variable terms.
     */
    override lazy val subsVar =
        rule {
            case Letp (ds, e1 @ Var (x)) =>
                ds find { case Bind (y, _, _) => x == y } match {
                     case None                   => e1
                     case Some (Bind (_, _, e2)) => e2
                }
        }

    /**
     * Substitution in applications.
     */
    override lazy val subsApp =
        rule {
            case Letp (ds, App (e1, e2)) =>
                App (Letp (ds, e1), Letp (ds, e2))
        }

    /**
     * Substitution in lambda abstractions.
     */
    override lazy val subsLam =
        rule {
            case Letp (ds, Lam (x, t, e)) =>
                val y = freshvar ()
                Lam (y, t, Letp (ds, Letp (List (Bind (x, t, Var (y))), e)))
        }

    /**
     * Substitution in primitive operations
     */
    override lazy val subsOpn =
        rule {
            case Letp (ds, Opn (op, e1, e2)) =>
                Opn (op, Letp (ds, e1), Letp (ds, e2))
        }

    /**
     * Merging two singleton parallel binders.
     */
    lazy val letLetOne =
        rule {
            case Letp (List (Bind (x, t1, e1)),
                       Letp (List (Bind (y, t2, e2)), e3)) =>
                Letp (List (Bind (y, t2, Letp (List (Bind (x, t1, e1)),
                                               e2)),
                            Bind (x, t1, e1)),
                      e3)
        }

    /**
     * Merging two arbitrary parallel binders.
     */
    lazy val letLet =
        rule {
            case Letp (ds1, Letp (ds2, e1)) =>
                val ds3 = ds2 map {
                    case Bind (x, t, e) => Bind (x, t, Letp (ds1, e))
                }
                val ds4 = ds3 ++ ds1
                Letp (ds4, e1)
        }

}

class ParEagerSubstEvaluator extends ParEagerSubst
