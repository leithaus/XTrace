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

import AST._
import TypeAnalysis._

/**
 * An environment to hold name bindings, nested within a parent environment.
 */
class Environment (parent: Environment) {

    import scala.collection.mutable.HashMap

    case class EnvEntry (dec : Declaration, multiplyDefined : Boolean)

    val decls = new HashMap [String, EnvEntry]

    /**
     * Add new entry to the environment.  Note: if the entry already exists, it is
     * overwritten by the new entry, but with it' 'multiplyDefined' field set to true
     */
    def addToEnv (nm : String, dec : Declaration) {
        val alreadyDefined = decls.contains (nm)
        decls += (nm -> EnvEntry (dec, alreadyDefined))
    }

    /**
     * Add declaration list to the environment
     */
    def addDeclsToEnv (decls : List[Declaration]) {
        decls.foreach (dec => addToEnv (dec.getName, dec))
    }

    /**
     * Find a declaration in the environment.  If a parent is specified, and 'searchAll'
     * is true, then also search parent environments
     */
    def findDecl (nm : String, searchAll : Boolean) : Declaration = {
        if (decls.contains (nm))
            decls (nm).dec
        else if (searchAll && (parent != null))
            parent.findDecl (nm, true)
        else
            UnknownDecl (nm)
    }

    /**
     * Return whether the name is multiply defined in this scope
     */
    def isMultiplyDefined (nm : String) : Boolean = {
        if (decls.contains (nm))
            decls (nm).multiplyDefined
        else
            false
    }

    override def toString = decls.toString
}

/**
 * Semantic attributes
 */
object NameAnalysis {

    import kiama.attribution.Attributable
    import kiama.attribution.Attribution._
    import TypeAnalysis._

    /**
     * Is the name already declared in this scope or not?
     */
    val isMultiplyDefined : Declaration ==> Boolean =
        attr {
            case dec : Declaration => (dec->env).isMultiplyDefined(dec.getName)
        }

    /**
     * The accessible names and types at a given point in the program.
     */
    val env : Attributable ==> Environment =
        attr {
            // Modules: Create a new environment
            case md @ ModuleDecl (id, decls, _, _, _) => {

                // Create a global environment (necessary for resolving function calls such
                // as <ModuleName>.<ProcName>)
                var globalEnv = new Environment (null)
//                globalEnv.addToEnv (id.name, md)
                globalEnv.addToEnv ("Write", BuiltInProcDecl ("Write", List (VarDecl ("exp", IntegerType)), ProcType (List (VarDecl ("exp", IntegerType)))))
                globalEnv.addToEnv ("WriteLn", BuiltInProcDecl ("WriteLn", List (VarDecl ("exp", IntegerType)), ProcType (List (VarDecl ("exp", IntegerType)))))
                globalEnv.addToEnv ("Read", BuiltInProcDecl ("Read", List (RefVarDecl ("var", IntegerType)), ProcType (List (RefVarDecl ("var", IntegerType)))))

                // Create module environment
                var env1 = new Environment (globalEnv)
                env1.addDeclsToEnv (decls)
                env1
            }

            // Procedures: Add the procedure to the parent env and create a new environment
            case p @ ProcDecl (id, fps, decls, _, _, _) => {
                var env1 = new Environment (p.parent->env)
                env1.addDeclsToEnv (fps)
                env1.addDeclsToEnv (decls)
                env1
            }

            // Records: Create a new environment
            case rt @ RecordType (fldlst) => {
                var env1 = new Environment (rt.parent->env)
                env1.addDeclsToEnv (fldlst)
                env1
            }

            // Other objects:  Get parent's environment
            case obj @ _ => obj.parent->env
        }

    /**
     * The declaration associated with each applied occurrence of an Ident.
     */
    val decl : Ident ==> Declaration =
        attr {
            case id @ Ident (nm) =>
                id.parent match {
                    // RHS of a field designation
                    case FieldDesig (left, id2) if (id == id2) => {
                        val rectp = left->objType
                        left->objType match { case InvalidType => UnknownDecl (nm)
                                              case _ => (rectp->env).findDecl (nm, false)
                                            }
                    }
                    // Other
                    case _ => (id->env).findDecl (nm, true)
                }
        }
}
