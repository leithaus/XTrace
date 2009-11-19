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
 * Move all declarations to the start of the program.
 */
trait TIL2_3 extends TIL1_1 with TransformingMain {

    import AST._

    override def transform (ast : Root) : Root = {
        var decls = new scala.collection.mutable.ListBuffer[Decl] ()
        val getandremovedecls =
            everywheretd (rule {
                case (d : Decl) :: ss =>
                    decls += d
                    ss
            })
        val Program (stmts) = rewrite (getandremovedecls) (ast)
        Program (decls.toList ++ stmts)
    }

}

object TIL2_3Main extends TIL2_3
