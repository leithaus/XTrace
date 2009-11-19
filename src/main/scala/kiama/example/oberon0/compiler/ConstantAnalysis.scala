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

object ConstantAnalysis {

    import kiama.attribution.Attribution._
    import AST._
    import NameAnalysis._

    /**
     * Is an expression constant or not?
     */
    val isConstant : Exp ==> Boolean =
        attr {
            case il : IntegerLiteral => true

            // Unary
            case Not (e) => e->isConstant

            case ue : UnaryNumExp => (ue.getExp)->isConstant

            // Binary
            case be : BinaryNumExp => (be.getLeft)->isConstant && (be.getRight)->isConstant
            case be : BinaryBoolExp => (be.getLeft)->isConstant && (be.getRight)->isConstant

            // Constants
            case id @ Ident (nm) if (id->decl).isInstanceOf[ConstDecl] => true

            // Other
            case _ => false
        }

}
