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

/**
 * This file is derived from a JastAdd implementation of PicoJava, created
 * in the Department of Computer Science at Lund University.  See the
 * following web site for details:
 *
 * http://jastadd.cs.lth.se/examples/PicoJava/index.shtml
 */

package kiama.example.picojava

object NameResolution {

    import AbstractSyntax._
    import NullObjects._
    import PredefinedTypes._
    import TypeAnalysis._
    import kiama.attribution.Attributable
    import kiama.attribution.Attribution._

    /**
     * decl refers to the appropriate declaration of the Access,
     * or to unknownDecl if the declaration is missing.
     *
     * syn lazy Decl Access.decl();
     * eq IdUse.decl() = lookup(getName());
     * eq Dot.decl() = getIdUse().decl();
     */
    val decl : Access ==> Decl =
        attr {
            case Dot (_, n) => n->decl
            case u : IdUse  => u->lookup (u.Name)
        }

    /**
     * Lookup a name.
     *
     * inh Decl IdUse.lookup(String name);
     * inh Decl Block.lookup(String name);
     * inh Decl TypeDecl.lookup(String name);
     *
     * eq Program.getBlock().lookup(String name) = localLookup(name); // lookup predefined types
     *
     * FIXME: haven't encoded this one, needed?
     * eq Program.getPredefinedType(int index).lookup(String name) = unknownDecl();
     *
     * eq Block.getBlockStmt(int index).lookup(String name) {
     *    // First, look in the local declarations
     *    if (!localLookup(name).isUnknown())
     *        return localLookup(name);
     *    // Then, look in surrounding context
     *    return lookup(name);
     * }
     *
     * eq ClassDecl.getBody().lookup(String name) {
     *    // First, look in superclass chain
     *    if (superClass() != null && !superClass().remoteLookup(name).isUnknown())
     *        return superClass().remoteLookup(name);
     *    // Then, look in surrounding context
     *    return lookup(name);
     * }
     *
     * eq Dot.getIdUse().lookup(String name) =
     *    // Do a remote lookup on the object's type.
     *    getObjectReference().decl().type().remoteLookup(name);
     */
    val lookup : String => Attributable ==> Decl =
        paramAttr {
            name => {
                case b : Block =>
                    b.parent match {
                        case p : Program   => p->localLookup (name)
                        case c : ClassDecl =>
                            if ((c->superClass != null) && (!isUnknown (c->superClass->remoteLookup (name))))
                                c->superClass->remoteLookup (name)
                            else
                                c->lookup (name)
                    }
                case s : BlockStmt =>
                    s.parent match {
                        case b : Block => {
                            val d = b->localLookup (name)
                            if (isUnknown (d)) b->lookup (name) else d
                        }
                        case p => p->lookup (name)
                    }
                case i : IdUse =>
                    i.parent match {
                        case Dot (a, `i`) => a->decl->tipe->remoteLookup (name)
                        case p            => p->lookup (name)
                    }
                case t =>
                    t.parent->lookup (name)
           }
       }

    /**
     * Look through the local declarations in a block.
     *
     * syn lazy Decl Block.localLookup(String name) {
     *     for (int k = 0; k < getNumBlockStmt(); k++) {
     *         Decl d = getBlockStmt(k).declarationOf(name);
     *         if (d != null) return d;
     *     }
     *     return unknownDecl();
     * }
     *
     * syn lazy Decl Program.localLookup(String name) {
     *     for (int k = 0; k < getNumPredefinedType(); k++) {
     *         Decl d = getPredefinedType(k).declarationOf(name);
     *         if (d != null) return d;
     *     }
     *     return unknownDecl();
     * }
     */
    val localLookup : String => Attributable ==> Decl =
        paramAttr {
            name => {
                case p : Program => finddecl (p, name, p->getPredefinedTypeList)
                case b : Block   => finddecl (b, name, b.BlockStmts)
            }
        }

    /**
     * Search a sequence of block statements for a declaration matching a given name.
     * Return the matching declaration or the unknown declaration if not found.
     */
    private def finddecl (t : Attributable, name : String, blockstmts : Seq[BlockStmt]) : Decl = {
         for (blockstmt <- blockstmts) {
             val d = blockstmt->declarationOf (name)
             if (d != null) return d
         }
         t->unknownDecl
    }

    /**
     * Perform a remote name lookup.
     *
     *  Looks through declarations of this type that are accessible from outside the type
     * By default, there are no such declarations, so return unknownDecl.
     *
     * syn Decl TypeDecl.remoteLookup(String name) = unknownDecl();
     *
     * eq ClassDecl.remoteLookup(String name) {
     *     // First, look in local declarations
     *     if (!getBody().localLookup(name).isUnknown())
     *         return getBody().localLookup(name);
     *     // Then, look in the superclass chain
     *     if (superClass() != null && !superClass().remoteLookup(name).isUnknown())
     *         return superClass().remoteLookup(name);
     *     // Otherwise, return null object unknown
     *     return unknownDecl();
     * }
     */
    val remoteLookup : String => TypeDecl ==> Decl =
        paramAttr {
            name => {
                case c : ClassDecl =>
                    if (!isUnknown (c.Body->localLookup (name)))
                        c.Body->localLookup (name)
                    else if ((c->superClass != null) && (!isUnknown (c->superClass->remoteLookup (name))))
                        c->superClass->remoteLookup (name)
                    else
                        c->unknownDecl
                case t =>
                    t->unknownDecl
            }
        }

    /**
     *
     * syn Decl BlockStmt.declarationOf(String name) = null;
     * eq Decl.declarationOf(String name) {
     *     if (getName().equals(name)) return this;
     *     return null;
     * }
     */
    val declarationOf : String => BlockStmt ==> Decl =
         paramAttr {
             name => {
                 case d : Decl => if (name == d.Name) d else null
                 case _        => null
             }
         }

}
