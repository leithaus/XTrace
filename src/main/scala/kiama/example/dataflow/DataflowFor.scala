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

package kiama.example.dataflow

import kiama.attribution.DynamicAttribution._
import DataflowAST._
import Dataflow._

case class Foreach (cond : Var, body : Stm) extends Stm

object DataflowForeach {

    Dataflow.succ +=
        attr { case t @ Foreach (_, body) => following (t) + body }

    // Using the childAttr notation
    Dataflow.following +=
        childAttr {
            _ => { case t @ Foreach(_, body) => following (t) + body }
        }

    // Alternatively, using the regular attr notation
    Dataflow.following +=
        attr {
            case t if t.parent.isInstanceOf[Foreach] =>
                val parent = t.parent[Foreach]
                following (parent) + parent.body
        }

}

case class For(init : Stm, c : Stm, inc : Stm, body : Stm) extends Stm

object DataflowFor {

    Dataflow.succ +=
        attr { case For (init, c, inc, body) => Set (init) }

    Dataflow.following +=
        childAttr {
            S => {
                case t @ For (S, c, _, _) => Set (c)
                case t @ For (_, S, _, b) => following (t) + b
                case t @ For (_, c, S, _) => Set (c)
                case t @ For (_, _, i, S) => Set (i)
            }
        }

}
