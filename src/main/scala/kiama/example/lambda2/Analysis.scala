/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009 Anthony M Sloane, Macquarie University.
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

package kiama.example.lambda2

/**
 * Analyses for typed lambda calculus expressions.  A simple free variable
 * analysis plus name and type analysis.  There are two versions of the 
 * latter here: one (tipe) that constructs an explicit environment separate
 * from the AST, and one (tipe2) that represents names by references to the
 * nodes of their binding lambda expressions.
 */
object Analysis {

    import AST._
    import kiama.attribution.Attribution._
    import kiama.util.Messaging._

    /**
     * The variables that are free in the given expression.
     */
    val fv : Exp ==> Set[Idn] =
        attr {
            case Num (_)         => Set ()
            case Var (v)         => Set (v)
            case Lam (v, _, e)   => fv (e) -- Set (v)
            case App (e1, e2)    => fv (e1) ++ fv (e2)
            case Opn (_, e1, e2) => fv (e1) ++ fv (e2)
        }

    /**
     * The environment of an expression is the list of variable names that
     * are visible in that expression and their types.
     */
    val env : Exp ==> List[(Idn,Type)] =
        childAttr {
            case _ => {

                // Nothing is visible at the root of the tree
                case null              => List ()

                // Inside a lambda expression the bound variable is now visible
                // in addition to everything that is visible from above. Note
                // that an inner declaration of a var hides an outer declaration
                // of the same var since we add inner bindings at the beginning
                // of the env and we search the env list below in tipe from
                // beginning to end
                case p @ Lam (x, t, _) => (x,t) :: p->env

                // Other expressions do not bind new identifiers so they just
                // get their environment from their parent
                case p : Exp           => p->env

            }
        }

    /**
     * The type of an expression.  Checks constituent names and types.  Uses
     * the env attribute to get the bound variables and their types.
     */
    val tipe : Exp ==> Type =
        attr {

            // A number is always of integer type
            case Num (_)          => IntType

            // An identifier is looked up in the environement of the current
            // expression.  If we find it, then we use the type that we find.
            // Otherwise it's an error.
            case e @ Var (x)      => (e->env).find { case (y,_) => x == y } match {
                                         case Some ((_, t)) => t
                                         case None =>
                                            message (e, "'" + x + "' unknown")
                                            IntType
                                     }

            // A lambda expression is a function from the type of its argument
            // to the type of the body expression
            case Lam (_, t, e)    => FunType (t, e->tipe)

            // For an application we first determine the type of the expression
            // being applied.  If it's a function whose argument type is the same
            // as the type of the argument in the application, then the type of
            // the application is the type of the body of the function.  If it's
            // a function but the argument types do not match, then it's an error.
            // If it's not a function then it's also an error.
            case App (e1, e2)     => e1->tipe match {
                                         case FunType (t1, t2) if t1 == e2->tipe => t2
                                         case FunType (t1, t2) =>
                                             message (e2, "expected " + t1 + ", found " + (e2->tipe))
                                             IntType
                                         case _ =>
                                             message (e1, "application of non-function")
                                             IntType
                                     }

            // An operation must be applied to two integers and returns an
            // integer.
            case Opn (op, e1, e2) => if (e1->tipe != IntType)
                                         message (e1, "expected Int, found " + (e1->tipe))
                                     if (e2->tipe != IntType)
                                         message (e2, "expected Int, found " + (e2->tipe))
                                     IntType
        }

    /**
     * For a given variable reference, return the lambda node that binds it if
     * there is one, otherwise return None.
     */
    def lookup (name : Idn) : Exp ==> Option[Lam] =
        attr {
            // Inside a lambda expression the bound variable is now visible
            // in addition to everything that is visible from above.  If
            // this lambda expression binds the name we are looking for, then
            // return this node.
            case e @ Lam (x, t, _) if x == name => Some (e)

            // Nothing is visible at the root of the tree
            case e if e isRoot                  => None


            // Other expressions do not bind new identifiers so they just
            // get their environment from their parent
            case e                              => e.parent[Exp]->lookup (name)
        }

    /**
     * The type of an expression.  Checks constituent names and types. Uses
     * the lookup attribute to get the lambda node that binds a name. For
     * other cases it behaves like tipe.
     */
    val tipe2 : Exp ==> Type =
        attr {

            // A number is always of integer type
            case Num (_)          => IntType

            // An identifier is looked up in the environement of the current
            // expression.  If we find it, then we use the type that we find.
            // Otherwise it's an error.
            case e @ Var (x) => (e->lookup (x)) match {
                                    case Some (Lam (_, t, _)) => t
                                    case None =>
                                        message (e, "'" + x + "' unknown")
                                        IntType
                                }

            // A lambda expression is a function from the type of its argument
            // to the type of the body expression
            case Lam (_, t, e)    => FunType (t, e->tipe2)

            // For an application we first determine the type of the expression
            // being applied.  If it's a function whose argument type is the same
            // as the type of the argument in the application, then the type of
            // the application is the type of the body of the function.  If it's
            // a function but the argument types do not match, then it's an error.
            // If it's not a function then it's also an error.
            case App (e1, e2)     => e1->tipe2 match {
                                         case FunType (t1, t2) if t1 == e2->tipe2 => t2
                                         case FunType (t1, t2) =>
                                             message (e2, "expected " + t1 + ", found " + (e2->tipe2))
                                             IntType
                                         case _ =>
                                             message (e1, "application of non-function")
                                             IntType
                                     }

            // An operation must be applied to two integers and returns an
            // integer.
            case Opn (op, e1, e2) => if (e1->tipe2 != IntType)
                                         message (e1, "expected Int, found " + (e1->tipe2))
                                     if (e2->tipe2 != IntType)
                                         message (e2, "expected Int, found " + (e2->tipe2))
                                     IntType

        }

}
