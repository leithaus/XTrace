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
 * Abstract syntax for Oberon-0.
 */
object AST {

    import TypeAnalysis._

    /**
     * Simple interface for pretty-printing capabilities.
     */
    trait PrettyPrintable {

        /**
         * Print tabs
         */
        def printTabs (o : StringBuilder, indent : Int) {
            var i: Int = 0

            while (i < indent) {
                o.append ("    ")
                i = i + 1
            }
        }

        /**
         * Pretty-print a list
         */
        def printList (o : StringBuilder, indent : Int, lst : List[PrettyPrintable], heading : String) {
            if (!lst.isEmpty) {
                printTabs (o, indent)
                o.append (heading)
                o.append ("List(\n")
                lst.foreach(obj => obj.pretty(o, indent + 3))
                printTabs (o, indent + 2)
                o.append (")\n")
            }
        }

        /**
         * Pretty-print the object at the end of the given string builder.
         */
        def pretty (o : StringBuilder, indent : Int) {
            printTabs (o, indent)
            o.append (this)
        }
    }

    abstract class Exp extends Attributable with PrettyPrintable

    abstract class Desig extends Exp
    case class Ident (name : String) extends Desig
    case class FieldDesig (left : Desig, id : Ident) extends Desig
    case class ArrayDesig (left : Desig, exp : Exp) extends Desig

    abstract class Literal extends Exp
    case class IntegerLiteral (num: Int) extends Literal

    abstract class UnaryNumExp (e : Exp) extends Exp {
        def getExp = e
        val op : Int => Int
    }

    case class Pos (e : Exp) extends UnaryNumExp (e) {
        override val op = {x : Int => x}
    }

    case class Neg (e : Exp) extends UnaryNumExp (e) {
        override val op = {x : Int => -x}
    }

    abstract class BinaryNumExp (l : Exp, r : Exp) extends Exp {
        def getLeft = l
        def getRight = r
        val op : (Int, Int) => Int
    }

    case class Mult (l : Exp, r : Exp) extends BinaryNumExp (l, r) {
        override val op = {(x : Int, y : Int) => x * y}
    }

    case class Div (l : Exp, r : Exp) extends BinaryNumExp (l, r) {
        override val op = {(x : Int, y : Int) => x / y}
    }

    case class Mod (l : Exp, r : Exp) extends BinaryNumExp (l, r) {
        override val op = {(x : Int, y : Int) => x % y}
    }

    case class Plus (l : Exp, r : Exp) extends BinaryNumExp (l, r) {
        override val op = {(x : Int, y : Int) => x + y}
    }

    case class Minus (l : Exp, r : Exp) extends BinaryNumExp (l, r) {
        override val op = {(x : Int, y : Int) => x - y}
    }

    case class Not (e : Exp) extends Exp

    abstract class BinaryBoolExp (l : Exp, r : Exp) extends Exp
    {
        def getLeft = l
        def getRight = r
    }

    case class And (l : Exp, r : Exp) extends BinaryBoolExp (l, r)
    case class Or (l : Exp, r : Exp) extends BinaryBoolExp (l, r)
    case class Equal (l : Exp, r: Exp) extends BinaryBoolExp (l, r)
    case class NotEqual (l : Exp, r: Exp) extends BinaryBoolExp (l, r)
    case class LessThan (l : Exp, r: Exp) extends BinaryBoolExp (l, r)
    case class LessThanOrEqual (l : Exp, r : Exp) extends BinaryBoolExp (l, r)
    case class GreaterThan (l : Exp, r : Exp) extends BinaryBoolExp (l, r)
    case class GreaterThanOrEqual (l : Exp, r : Exp) extends BinaryBoolExp (l, r)

    abstract class Statement extends Attributable with PrettyPrintable {
        override def pretty (o : StringBuilder, indent : Int) {
            super.pretty (o, indent)
            o.append ("\n")
        }
    }

    case class Assignment (desig : Desig, exp : Exp) extends Statement
    case class ProcedureCall (desig : Exp, aps : List[Exp]) extends Statement

    case class IfStatement (condexp : Exp, thenstmts : List[Statement], elsestmts: List[Statement]) extends Statement {
        override def pretty (o : StringBuilder, indent : Int) {
            printTabs (o, indent)
            o.append ("IfStatement(condexp = ")
            condexp.pretty (o, 0)
            o.append ("\n")

            printList (o, indent + 1, thenstmts, "thenstmts = ")
            printList (o, indent + 1, elsestmts, "elsestmts = ")

            printTabs (o, indent)
            o.append (" )\n")
        }
    }

    case class WhileStatement (condexp : Exp, bodystmts : List[Statement]) extends Statement

    abstract class Declaration (name : String) extends Attributable with PrettyPrintable {

        var byteOffset : Int = -999		// Should get overwritten.

        override def pretty (o : StringBuilder, indent : Int) {
            super.pretty(o, indent)
            o.append("\n")
        }

        def getName = name
    }

    case class ConstDecl (name : String, constval : Exp) extends Declaration (name)
    case class VarDecl (name : String, tp : Type) extends Declaration (name)
    case class RefVarDecl (name : String, tp : Type) extends Declaration (name)
    case class TypeDecl (name : String, tp : Type) extends Declaration (name)
    case class FieldDecl (name : String, tp : Type) extends Declaration (name)

    case class ModuleDecl (name : String, decls : List[Declaration], stmts : List[Statement], name2 : String, tp : ModuleType) extends Declaration (name) {
        var byteSize = -999

        override def pretty (o : StringBuilder, indent : Int) {
            o.append ("ModuleDecl(id = " + name + "\n")
            printList (o, indent + 1, decls, "decls = ")
            printList (o, indent + 1, stmts, "stmts = ")
            printTabs (o, indent + 1)
            o.append ("id2 = " + name2 + ")\n")
        }
    }

    case class ProcDecl (name : String, fps : List[Declaration], decls : List[Declaration], stmts : List[Statement], name2 : String, tp: ProcType) extends Declaration (name) {
      var byteSize = -999
      var label : Int = 0

      override def pretty (o : StringBuilder, indent : Int) {
            printTabs (o, indent)
            o.append ("ProcDecl(id = " + name + "\n")
            printList (o, indent + 1, fps, "fps = ")
            printList (o, indent + 1, decls, "decls = ")
            printList (o, indent + 1, stmts, "stmts = ")
            printTabs (o, indent + 1)
            o.append ("id2 = " + name2 + ")\n")
        }
    }

    case class BuiltInProcDecl (name : String, fps : List[Declaration], tp: ProcType) extends Declaration (name)

    case class UnknownDecl (name : String) extends Declaration(name)

    abstract class Type extends Attributable with PrettyPrintable

    case class NamedType (id : Ident) extends Type {
        override def toString = id.name
    }
    case class ArrayType (size : Exp, tp : Type) extends Type {
        override def toString = "ARRAY"
    }
    case class RecordType (fldlst : List[FieldDecl]) extends Type {
        override def toString = "RECORD"
    }
    case class ProcType (fps : List[Declaration]) extends Type {
        override def toString = "PROCEDURE"
    }
    case class ModuleType () extends Type {
        override def toString = "MODULE"
    }

    case object IntegerType extends Type {
        override def toString = "INTEGER"
    }
    case object BooleanType extends Type {
        override def toString = "BOOLEAN"
    }
    case object InvalidType extends Type {
        override def toString = "unknown type"
    }
    case object StatementType extends Type {
        override def toString = "statement"
    }
    
}
