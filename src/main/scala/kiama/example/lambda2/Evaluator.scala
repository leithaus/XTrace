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

import AST._

/**
 * Interface for a lambda2 evaluator.
 */
trait Evaluator {
    
    /**
     * Evaluate the given expression, returning the result of the
     * evaluation if it succeeded, or exp if it failed.
     */
    def eval (exp : Exp) : Exp

    /**
     * Whether this mechanism evaluates inside lambdas.  Used for
     * testing.  Default: false.
     */
    def reducesinlambdas = false

    /**
     * Generate a fresh variable name.  Prefix the name with an underscore
     * to avoid the potential for clashes with user-level varaibles (which
     * must start with a letter).
     */
    object freshvar {
        private var count = 0
        def apply () : String = {
            count = count + 1
            "_v" + count
        }
    }
    
    /**
     * Capture-free substitution of free occurrences of x in e1 with e2.
     */
    def substitute (x : Idn, e2: Exp, e1 : Exp) : Exp =
        e1 match {
            case Var (y) if x == y =>
                e2
            case Lam (y, t, e3) =>
                val z = freshvar ()
                Lam (z, t, substitute (x, e2, substitute (y, Var (z), e3)))
            case App (l, r) =>
                App (substitute (x, e2, l), substitute (x, e2, r))
            case Opn (op, l, r) =>
                Opn (op, substitute (x, e2, l), substitute (x, e2, r))
            case e =>
                e
        }
    
}

/**
 * Interface for an individual rewriting-based lambda2 evaluator.
 */
trait RewritingEvaluator extends Evaluator with kiama.rewriting.Rewriter {
    
    /**
     * Evaluate the given expression by rewriting it with the evals
     * strategy.
     */
    def eval (exp : Exp) : Exp =
        rewrite (evals) (exp)

    /**
     * The strategy to use to perform the evaluation.
     */
    val evals : Strategy
    
}
