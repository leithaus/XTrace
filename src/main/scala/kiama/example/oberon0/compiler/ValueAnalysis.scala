/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009 Anthony M Sloane, Macquarie University.
 *
 * Contributed by Ben Mockler.
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

package kiama.example.oberon0.compiler

/**
 * Value analysis for Oberon0 expressions.
 */
object ValueAnalysis {

    import kiama.attribution.Attribution._
    import AST._
    import NameAnalysis._
    import TypeAnalysis._

    /**
     * The integer value of an expression that is assumed to be of IntegerType
     * and constant.
     */
    val intValue : Exp ==> Int =
        attr {
            case IntegerLiteral (num) => num

            case id : Ident => id->decl match {
                case ConstDecl (_, valexp) => valexp->intValue
            }

            case Pos (e) => e->intValue

            case Neg (e) => -(e->intValue)

            case be : BinaryNumExp => be.op ((be.getLeft)->intValue, (be.getRight)->intValue)
        }

    /**
     * The Boolean value of an expression that is assumed to be of BooleanType
     * and constant.
     */
    val boolValue : Exp ==> Boolean =
        attr {
            case Not (e) => !(e->boolValue)

            case And (l, r) => (l->boolValue) && (r->boolValue)

            case Or (l, r) => (l->boolValue) || (r->boolValue)

            case Equal (l, r) if l->objType == BooleanType => (l->boolValue) == (r->boolValue)
            case Equal (l, r) if l->objType == IntegerType => (l->intValue) == (r->intValue)

            case NotEqual (l, r) if l->objType == BooleanType => (l->boolValue) != (r->boolValue)
            case NotEqual (l, r) if l->objType == IntegerType => (l->intValue) != (r->intValue)

            case LessThan (l, r) => (l->intValue) < (r->intValue)

            case LessThanOrEqual (l, r) => (l->intValue) <= (r->intValue)

            case GreaterThan (l, r) => (l->intValue) > (r->intValue)

            case GreaterThanOrEqual (l, r) => (l->intValue) >= (r->intValue)
        }
}
