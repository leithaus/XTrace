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
 * Interface to switchable evaluators for the lambda2 language.  Enables
 * a client to select an evaluation mechanism by name and access the
 * evaluator for that mechanism.
 */
object Evaluators {

  /**
     * Map of evaluator names to the evaluators themselves.
     * Coments refer to names in Dolstra and Visser paper.
     */
    private val evalmap =
        Map ("reduce"         -> new ReduceEvaluator, 	       // eval1
             "reducesubst"    -> new ReduceSubstEvaluator,     // eval2
             "innermostsubst" -> new InnermostSubstEvaluator,  // eval3
             "eagersubst"     -> new EagerSubstEvaluator,      // eval4, eval5
             "lazysubst"      -> new LazySubstEvaluator,       // eval6
             "pareagersubst"  -> new ParEagerSubstEvaluator,   // eval7
             "parlazysubst"   -> new ParLazySubstEvaluator)    // eval8

    /**
     * Return the names of the available evaluation mechanisms.
     */
    val mechanisms = evalmap.keySet

    /**
     * The name of the current evaluation mechanism (default: "reduce")
     */
    var mechanism = "reduce"

    /**
     * The current evaluation mechanism.
     */
    var evaluator = evalmap (mechanism)

    /**
     * Change the current evaluation mechanism to that denoted by mech
     * if it is a legal one, otherwise do nothing.  Returns whether the
     * change could be made or not.
     */
    def setEvaluator (mech : String) : Boolean =
        if (mechanisms contains mech) {
            mechanism = mech
            evaluator = evalmap (mech)
            true
        } else
            false

}
