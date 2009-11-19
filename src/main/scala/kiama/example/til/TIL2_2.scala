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
 * Transform for loops into equivalent while loops.
 */
trait TIL2_2 extends TIL1_1 with TransformingMain {

    import AST._

    override def transform (ast : Root) : Root =
        rewrite (fortowhile) (ast)

    val fortowhile =
        everywheretd (rule {
            case (s @ For (id @ Id (i), f, t, b)) :: ss =>
                val upperid = Id ("Upper" + i)
                Decl (id) ::
                Assign (id, f) ::
                Decl (upperid) ::
                Assign (upperid, Add (t, Num (1))) ::
                While (Sub (Var (id), Var (upperid)),
                    b ++ List (Assign (id, Add (Var (id), Num (1))))) ::
                ss
        })

}

object TIL2_2Main extends TIL2_2
