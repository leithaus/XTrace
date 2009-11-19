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

/**
 * Control flow interface.
 */
trait ControlFlow {

    /**
     * Control flow successor relation.
     */
    val succ : Stm ==> Set[Stm]


    /**
     * Control flow default successor relation.
     */
    val following : Stm ==> Set[Stm]

}

/**
 * Control flow implementation.
 */
trait ControlFlowImpl extends ControlFlow {

    val succ : Stm ==> Set[Stm] =
        attr {
            case If (_, s1, s2)   => Set (s1, s2)
            case t @ While (_, s) => t->following + s
            case Return (_)       => Set ()
            case Block (s, _*)    => Set (s)
            case s                => s->following
        }

    val following : Stm ==> Set[Stm] =
        childAttr {
            case s => {
                 case t @ While (_, _)           => Set (t)
                 case b @ Block (_*) if s isLast => b->following
                 case Block (_*)                 => Set (s.next)
                 case _                          => Set ()
            }
        }

}

/**
 * Variable use and definition interface.
 */
trait Variables {

    /**
     * Variable uses.
     */
    val uses : Stm ==> Set[Var]

    /**
     * Variable definitions.
     */
    val defines : Stm ==> Set[Var]

}

/**
 * Variable use and definition implementation.
 */
trait VariablesImpl extends Variables {

    val uses : Stm ==> Set[Var] =
        attr {
            case If (v, _, _)  => Set (v)
            case While (v, _)  => Set (v)
            case Assign (_, v) => Set (v)
            case Return (v)    => Set (v)
            case _             => Set ()
        }

    val defines : Stm ==> Set[Var] =
        attr {
            case Assign (v, _) => Set (v)
            case _             => Set ()
        }

}

/**
 * Variable liveness interface.
 */
trait Liveness {

    /**
     * Variables "live" into a statement.
     */
    val in : Stm ==> Set[Var]

    /**
     * Variables "live" out of a statement.
     */
    val out : Stm ==> Set[Var]

}

/**
 * Variable liveness implementation.
 */
trait LivenessImpl extends Liveness {

    self : Liveness with Variables with ControlFlow =>

    val in : Stm ==> Set[Var] =
        circular (Set[Var]()) {
            case s => uses (s) ++ (out (s) -- defines (s))
        }

    val out : Stm ==> Set[Var] =
        circular (Set[Var]()) {
            case s => (s->succ) flatMap (in)
        }

}

object Dataflow extends LivenessImpl with VariablesImpl with ControlFlowImpl
