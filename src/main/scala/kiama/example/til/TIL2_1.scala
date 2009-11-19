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

package kiama.example.til

import kiama.rewriting.Rewriter

/**
 * Rewrite TILs for loops that automatically declare the control variable
 * adding an explicit declaration of the variable.
 */
trait TIL2_1 extends TIL1_1 with TransformingMain {

    import AST._

    override def transform (ast : Root) : Root =
        rewrite (declareforvars) (ast)

    val declareforvars =
        everywherebu (rule {
            case (s @ For (Id (i), f, t, b)) :: ss =>
                Decl (Id (i)) :: s :: ss
        })

}

object TIL2_1Main extends TIL2_1
