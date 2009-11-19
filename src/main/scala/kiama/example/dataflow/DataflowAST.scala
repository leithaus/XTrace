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

import kiama.attribution.Attributable

/**
 * Imperative language AST for dataflow example.
 */
object DataflowAST {
    type Var = String
    case class Program (body : Stm) extends Attributable
    abstract class Stm extends Attributable
    case class Assign (left : Var, right : Var) extends Stm
    case class While (cond : Var, body : Stm) extends Stm
    case class If (cond : Var, tru : Stm, fls : Stm) extends Stm
    case class Block (stms : Stm*) extends Stm
    case class Return (ret : Var) extends Stm
    case class Empty () extends Stm
}
