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
 * Evaluation of lambda calculus using global beta reduction with
 * term-level substitution and arithmetic operations.
 */
trait ReduceSubst extends Reduce {

    import AST._

    /**
     * Evaluate by repeatedly trying to apply beta reduction and arithmetic
     * operators anywhere.
     */
    override lazy val evals =
        reduce (lambda)

    /**
     * Reusable strategy for reduction with explicit term-level substitution.
     */
    lazy val lambda =
        beta + arithop + subsNum + subsVar + subsApp + subsLam + subsOpn

    /**
     * Beta reduction via term-level substitution.
     */
    override lazy val beta =
        rule {
            case App (Lam (x, t, e1), e2) =>  Let (x, t, e2, e1)
        }

    /**
     * Substitution in numeric terms.
     */
    lazy val subsNum =
        rule {
            case Let (_, _, _, e : Num) => e
        }

    /**
     * Substitution in variable terms.
     */
    lazy val subsVar =
        rule {
            case Let (x, _, e, Var (y)) if x == y => e
            case Let (_, _, _, v : Var)           => v
        }

    /**
     * Substitution in applications.
     */
    lazy val subsApp =
        rule {
            case Let (x, t, e, App (e1, e2)) =>
                App (Let (x, t, e, e1), Let (x, t, e, e2))
        }

    /**
     * Substitution in lambda abstractions.
     */
    lazy val subsLam =
        rule {
            case Let (x, t1, e1, Lam (y, t2, e2)) if x == y =>
                Lam (y, t2, e2)
            case Let (x, t1, e1, Lam (y, t2, e2)) =>
                val z = freshvar ()
                Lam (z, t2, Let (x, t1, e1, Let (y, t2, Var (z), e2)))
        }

    /**
     * Substitution in primitive operations
     */
    lazy val subsOpn =
        rule {
            case Let (x, t, e1, Opn (op, e2, e3)) =>
                Opn (op, Let (x, t, e1, e2), Let (x, t, e1, e3))
        }

}

class ReduceSubstEvaluator extends ReduceSubst  {
    override def reducesinlambdas = true
}


