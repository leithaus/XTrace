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

import kiama.attribution.Attributable
import kiama.attribution.Attribution._

/**
 * Drive the semantic analysis of a program by checking error conditions.
 */
object ErrorCheck {

    import AST._
    import NameAnalysis._
    import TypeAnalysis._
    import ValueAnalysis._
    import ConstantAnalysis._
    import kiama.util.Messaging.message

    /**
     * Collect the errors at a node or its children.  The errors are recorded
     * using Kiama's Messaging module.
     */
    val collectErrors : Attributable ==> Unit =
        attr {
            case obj => {
                // Process the errors of the children of t
                for (child <- obj.children)
                    collectErrors(child)

                // Process the errors at t
                obj match {

                    // Check for name-mismatch errors in module declarations
                    case md @ ModuleDecl (nm, _, _, nm2, _) if nm != nm2 =>
                        message (md, "name mismatch: opening identifier = " + nm +
                                 ", closing identifier = " + nm2)

                    // Check for name-mismatch errors in procedure declarations
                    case pd @ ProcDecl (nm, _, _, _, nm2, _) if nm != nm2 =>
                        message (pd, "name mismatch: opening identifier = " + nm +
                                 ", closing identifier = " + nm2)

                    // Check for undeclared identifiers (applied occurrences only)
                    case id @ Ident (nm) if ((id->decl).isInstanceOf[UnknownDecl]) =>
                        message (id, "declaration not found: " + nm)

                    // Check for duplicate declarations
                    case dec : Declaration if dec->isMultiplyDefined =>
                        message (dec, "duplicate declaration = " + dec) // FIXME dec.getName)
                        dec match {
                            case v @ VarDecl (_, _) => message (v, "hello")
                            case _ =>
                        }

                    // Check for incompatible types on either side of assignment statement
                    case as @ Assignment (desig, exp) if as->objType == InvalidType =>
                        message (as, "type mismatch in assignment expression: " + (desig->objType)
                                 + " expected, got " + (exp->objType))

                    // Check for non-integer size expression in array type declaration
                    case at @ ArrayType (sz, _) if sz->objType !=  IntegerType =>
                        message (at, "non-integer array size expression")

                    // Check for non-constant size expression in array type declaration
                    case at @ ArrayType (sz, _) if !(sz->isConstant) =>
                        message (at, "non-constant array size expression")

                    // Check for negative size expression in array type declaration
                    case at @ ArrayType (sz, _) if sz->intValue < 0 =>
                        message (at, "negative array size expression")

                    // Check for non-integer index expression in array designation
                    case ad @ ArrayDesig (_, exp) if exp->objType !=  IntegerType =>
                        message (ad, "non-integer array index expression")

                    // Check for constant, negative index expression in array designation
                    case ad @ ArrayDesig (_, exp) if (exp->isConstant && (exp->intValue < 0)) =>
                        message (ad, "negative array index expression")

                    // Check for constant, out-of-bounds index expression in array designation
                    case ad @ ArrayDesig (left, exp) if exp->isConstant =>
                        left->objType match {
                            case ArrayType (sz, _) => {
                                if (exp->intValue >= sz->intValue)
                                    message (ad, "out-of-bounds array index expression")
                            }
                            case _ => 
                                error ("non-array found in ArrayDesig")
                        }

                    // Check procedure call is on an actual procedure
                    case ProcedureCall (desig, _) if !(desig->objType).isInstanceOf[ProcType] =>
                        message (desig, "call of non-procedure")

                    // The procedure name must be a plain identifier
                    case pc @ ProcedureCall (desig, _) =>

                        desig match {
                            case fd : FieldDesig =>
                                message (fd, "procedure calls cannot use field designators")
                            case ad : ArrayDesig =>
                                message (ad, "Procedure calls cannot use array designators")
                            case id : Ident => {
                                // The procedure called must be a child or sibling of the
                                // current procedure
                                if (!(id->decl).isInstanceOf[BuiltInProcDecl])
                                    if (id->decl->level < id->level)
                                        message (pc, "procedure is not a child or sibling of caller")

                                // Check procedure call actual params against formal params
                                pc->procArgErrors
                            }

                            case _ => ()
                        }

                    // Check If-statement expressions are boolean
                    case IfStatement (condexp, _, _) if condexp->objType != BooleanType =>
                        message (condexp, "boolean expression expected")

                    // Check While-statement expressions are boolean
                    case ws @ WhileStatement (condexp, _) if condexp->objType != BooleanType =>
                        message (condexp, "boolean expression expected")

                    case _ =>
            }

        }
    }
}
