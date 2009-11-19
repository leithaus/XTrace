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

package kiama.example.repmin

import kiama.attribution.Attributable
import kiama.attribution.Attribution._

/**
 * AST for Repmin examples.
 */
abstract class Tree extends Attributable
case class Pair (left : Tree, right : Tree) extends Tree
case class Leaf (value : Int) extends Tree

/**
 * Classic repmin problem defined in an "attributes first" style.
 * repmin is a tree with the same structure as its argument tree
 * but with all of the leaves replaced by leaves containing the
 * minimum leaf value from the input tree.
 */
object Repmin {

    val locmin : Tree ==> Int =
        attr {
            case Pair (l, r) => (l->locmin) min (r->locmin)
            case Leaf (v)    => v
        }

    val globmin : Tree ==> Int =
        attr {
            case t if t isRoot => t->locmin
            case t             => t.parent[Tree]->globmin
        }

    val repmin : Tree ==> Tree =
        attr {
            case Pair (l, r) => Pair (l->repmin, r->repmin)
            case t : Leaf    => Leaf (t->globmin)
        }

}
