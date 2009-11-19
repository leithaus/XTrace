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

/**
 * PicoJava abstract syntax
 */
object AbstractSyntax {

    import kiama.attribution.Attributable
    import kiama.attribution.Attribution._

    // Created by parser
    case class Program (Block : Block) extends Attributable

    case class Block (BlockStmts : Seq[BlockStmt]) extends Attributable
    abstract class BlockStmt extends Attributable

    abstract class Decl (val Name : String) extends BlockStmt
    abstract class TypeDecl (Name : String) extends Decl (Name)
    case class ClassDecl (override val Name : String, Superclass : Option[IdUse], Body : Block) extends TypeDecl (Name)
    case class VarDecl (override val Name : String, Type : Access) extends Decl (Name)

    abstract class Stmt extends BlockStmt
    case class AssignStmt (Variable : Access, Value : Exp) extends Stmt
    case class WhileStmt (Condition : Exp, Body : Stmt) extends Stmt

    abstract class Exp extends Attributable
    abstract class Access extends Exp
    abstract class IdUse (val Name : String) extends Access

    case class Use (override val Name : String) extends IdUse (Name)
    case class Dot (ObjectReference : Access, IdUse : IdUse) extends Access
    case class BooleanLiteral (Value : String) extends Exp

    // Created by NTA equations
    case class PrimitiveDecl (override val Name : String) extends TypeDecl (Name)
    case class UnknownDecl (override val Name : String) extends TypeDecl (Name)

}
