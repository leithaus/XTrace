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
 * Build the assembly/RISC code for the program
 */
object Encoder {

    import AST._
    import NameAnalysis._
    import TypeAnalysis._
    import ConstantAnalysis._
    import ValueAnalysis._
    import kiama.attribution.Attributable
    import kiama.attribution.Attribution._
    import kiama.example.oberon0.assembler._
    import kiama.example.oberon0.machine.RISCISA._

    /**
     * For objects which require memory (VarDecls, RefVarDecls and FieldDecls), set byteOffset
     * For module and proc decls, set the total byteSize (re how much stack space required)
     * For procs, also assign a label
     */
    def setByteOffsets (obj : Attributable, byteOffset : Int) : Int = {

        obj match {

            case md @ ModuleDecl (_, decls, _, _, _) => {
                var varbyteOffset : Int = 0
                for (dec <- decls) {
                    varbyteOffset = setByteOffsets (dec, varbyteOffset)
                }
                md.byteSize = varbyteOffset
                byteOffset
            }

            case pd @ ProcDecl (_, fps, decls, _, _, _) => {
                var varbyteOffset : Int = 0
                for (fp <- fps) {
                    varbyteOffset = setByteOffsets (fp, varbyteOffset)
                }
                for (dec <- decls) {
                    varbyteOffset = setByteOffsets (dec, varbyteOffset)
                }
                pd.byteSize = varbyteOffset
                pd.label = Assembler.newlabel
                byteOffset					// The proc does not affect the byteSize of the
                                            // enclosing module or proc
            }

            case RecordType (fldlst) => {
                var varbyteOffset : Int = 0
                for (fld <- fldlst) {
                    varbyteOffset = setByteOffsets (fld, varbyteOffset)
                }
                byteOffset					// The record type does not affect the byteSize of
                                            // the enclosing module or proc
            }

            case TypeDecl (_, tp) => {
                setByteOffsets (tp, -999)   // -999 is a nonsense value.  Should not get used,
                                            // and will crash the program if it does.
                byteOffset					// The (named) type does not affect the byteSize of
                                            // the enclosing module or proc
            }

            case vd @ VarDecl (_, tp) => {
                vd.byteOffset = byteOffset
                setByteOffsets (tp, -999)
                byteOffset + (vd->objType->byteSize)
            }

            case fd @ FieldDecl (_, tp) => {
                fd.byteOffset = byteOffset
                setByteOffsets (tp, -999)
                byteOffset + (fd->objType->byteSize)
            }

            case rvd @ RefVarDecl (_, tp) => {
                rvd.byteOffset = byteOffset
                setByteOffsets (tp, -999)
                byteOffset + 4				// Ref vars always store a 4 byte address,
                                            // regardless of the object pointed-to
            }

            case _ => byteOffset

        }
    }

    /**
     * Encode a statement
     */
    def EncodeStatement (stmt : Statement, procOrModDecl : Declaration) {
        stmt match {
            case Assignment (desig, exp) =>
                EncodeAssignment (desig, exp, procOrModDecl)

            case IfStatement (condexp, thenstmts, elsestmts) =>
                EncodeIfStmt (condexp, thenstmts, elsestmts, procOrModDecl)

            case WhileStatement (condexp, bodystmts) =>
                EncodeWhileStmt (condexp, bodystmts, procOrModDecl)

            case ProcedureCall (desig, aps) =>
                EncodeProcedureCall (desig, aps, procOrModDecl)
        }
    }

    /**
     * Encode a module
     */
    def EncodeModule (md : ModuleDecl) {

        // Set memory usage data for the whole program
        setByteOffsets (md, -999)

        // Set the frame pointer (to 0)
        Assembler.emit (ADDI (29, 0, 0))

        // Set the stack pointer
        Assembler.emit (ADDI (30, 0, md.byteSize))

        // Encode each statement
        md.stmts.foreach (stmt => EncodeStatement(stmt, md))

        // Encode a RET(0) statement
        Assembler.emit (RET (0))

        // Encode all procedure decls
        val pdlst = md.decls.filter (dec => dec.isInstanceOf[ProcDecl])
        pdlst.foreach (dec => EncodeProc (dec.asInstanceOf[ProcDecl]))

    }

    /**
     * Encode a procedure
     */
    def EncodeProc (pd : ProcDecl) {

        // Emit the label
        Assembler.mark (pd.label)

        // Backup the return address to the stack
        Assembler.emit (PSH (31, 30, 4))

        // Backup the FP to the stack
        Assembler.emit (PSH (29, 30, 4))

        // Set the FP to the current SP
        Assembler.emit (ADDI (29, 30, 0))

        // Set the SP to the current SP + the frame size
        Assembler.emit (ADDI (30, 30, pd.byteSize))

        // Encode each statement
        pd.stmts.foreach (stmt => EncodeStatement (stmt, pd))

        // Set SP to current FP
        Assembler.emit (ADDI (30, 29, 0))

        // Restore previous FP
        Assembler.emit (POP (29, 30, 4))

        // Restore return address
        Assembler.emit (POP (31, 30, 4))

        // Discard the stored SL
        Assembler.emit (ADDI (30, 30, -4))

        // Encode a RET(0) statement
        Assembler.emit (RET (31))

        // Encode all procedure decls
        val pdlst = pd.decls.filter (dec => dec.isInstanceOf[ProcDecl])
        pdlst.foreach (dec => EncodeProc (dec.asInstanceOf[ProcDecl]))

    }

    /**
     * Return-value of processDesig procedure
     */
    case class desigResult (regno : Byte, offset : Int)

    /**
     * Process an ident designator
     */
    def processIdent (id : Ident, procOrModDecl : Declaration) : desigResult = {

        var baseAddrReg : Byte = 0

        // If a global variable ...
        if (id->decl->level == 0)
            // Start at memory location 0
            baseAddrReg = 0

        // If the id belongs to this proc or module ...
        else if ((id->decl).parent == procOrModDecl)
            // Start in this frame
            baseAddrReg = 29

        // If at an intermediate level ...
        else {
            // Load the static link (stored at FP - 12) into a register
            // ie. Get the base address of the statically enclosing stack frame
            baseAddrReg = Assembler.getFreeReg

            Assembler.emit (LDW (baseAddrReg, 29, -12))

            // Traverse further if necessary
            val steps = (procOrModDecl->level) - (id->decl->level)
            var i : Int = 1;

            while (i < steps) {
                Assembler.emit (LDW (baseAddrReg, baseAddrReg, -12))
                i = i + 1
            }
        }

        // Handle reference parameter redirection
        id->decl match {

            // Load address into an available register
            case rvd : RefVarDecl => {
                var reg : Byte = 0

                if (baseAddrReg == 0 || baseAddrReg == 29)
                    reg = Assembler.getFreeReg
                else
                    reg = baseAddrReg

                // Load the address
                Assembler.emit (LDW (reg, baseAddrReg, rvd.byteOffset))

                desigResult (reg, 0)
            }

            case _ => desigResult (baseAddrReg, (id->decl).byteOffset)
        }
    }

    /**
     * Process an array designator
     */
    def processArrayDesig (ad : ArrayDesig, left : Desig, exp : Exp,
                           procOrModDecl : Declaration) : desigResult = {

        val leftResult = processDesig (left, procOrModDecl)

        // Constant index
        if (exp->objType == IntegerType && exp->isConstant) {
            desigResult (leftResult.regno,
                         leftResult.offset + (exp->intValue) * (ad->objType->byteSize))
        }
        // Variable index
        else {

            // Process exp
            val tempReg = processNumExp (exp, procOrModDecl)

            // Multiply by array item size
            Assembler.emit ( MULI (tempReg, tempReg, (ad->objType->byteSize)))

            var dstReg : Byte = 0

            // If no reg currently allocated, can use tempReg
            if (leftResult.regno == 0) {
                dstReg = tempReg
            }
            // If FP is current base, can use tempReg, but add on the address in FP
            else if (leftResult.regno == 29) {
                dstReg = tempReg
                Assembler.emit ( ADD (dstReg, dstReg, 29))
            }
            // Otherwise use the same reg as the LHS (but need to add index result to it)
            else {
                dstReg = leftResult.regno

                // Add the result of the index calculation to this register
                Assembler.emit ( ADD (dstReg, dstReg, tempReg))
                Assembler.freeReg (tempReg)
            }

            desigResult (dstReg, leftResult.offset)
        }
    }

    /**
     * Resolve a designation into a memory address (register contents + offset)
     */
    def processDesig (des : Desig, procOrModDecl : Declaration) : desigResult = {

        des match {

            // Identifier
            case id : Ident =>
                processIdent (id, procOrModDecl)

            // Field designation
            case FieldDesig (left, id) =>
                val leftResult = processDesig (left, procOrModDecl)
                desigResult (leftResult.regno, leftResult.offset + (id->decl).byteOffset)

            // Array designation
            case ad @ ArrayDesig (left, exp) =>
                processArrayDesig (ad, left, exp, procOrModDecl)
        }
    }

    /**
     * Resolve a numeric expression into a register number
     */
    def processNumExp (exp : Exp, procOrModDecl : Declaration) : Byte = {

        var reg : Byte = 0

        // If the expression is constant, put value in a register
        if (exp->isConstant) {
            reg = Assembler.getFreeReg

            if (exp->objType == IntegerType)
                Assembler.emit ( ADDI (reg, 0, exp->intValue))
            else if (exp->objType == BooleanType)
                Assembler.emit ( ADDI (reg, 0, (exp->boolValue).asInstanceOf[Int]))

            return reg
        }

        // If the expression is non-constant, build it at runtime
        // If a designator ...
        exp match {

            case des : Desig => {
                // Get memory address as register and offset
                val desResult = processDesig (des, procOrModDecl)

                // If no register allocated, allocate one
                if (desResult.regno == 0 || desResult.regno == 29)
                    reg = Assembler.getFreeReg
                else
                    reg = desResult.regno

                // Load the item from memory
                Assembler.emit ( LDW (reg, desResult.regno, desResult.offset))

                reg
            }

            // Other expressions
            case ue : UnaryNumExp => {
                reg = processNumExp (ue.getExp, procOrModDecl)

                exp match {
                    case n : Neg => Assembler.emit ( MULI (reg, reg, -1))
                    case p : Pos => ()		// Do nothing
                }
                reg
            }

            case be : BinaryNumExp => {
                reg = processNumExp (be.getLeft, procOrModDecl)
                val rreg = processNumExp (be.getRight, procOrModDecl)

                exp match {
                    case m : Mult => Assembler.emit (MUL (reg, reg, rreg))
                    case d : Div => Assembler.emit (DIV (reg, reg, rreg))
                    case m : Mod => Assembler.emit (MOD (reg, reg, rreg))
                    case p : Plus => Assembler.emit (ADD (reg, reg, rreg))
                    case m : Minus => Assembler.emit (SUB (reg, reg, rreg))
                }

                Assembler.freeReg (rreg)
                reg
            }
        }
    }

    /**
     * processBoolExp (saying where to go if true)
     */
    def processBoolExp (exp : Exp, truelbl : Int, negate : Boolean, procOrModDecl : Declaration) {

        exp match {

            // Not
            case Not (e) => processBoolExp (e, truelbl, true, procOrModDecl)

            // And: Uses short-circuit evaluation
            case And (l, r) =>

                if (negate) {
                    processBoolExp (Or (Not (l), Not(r)), truelbl, false, procOrModDecl)
                }
                else {
                    val truelbl2 = Assembler.newlabel
                    val andendlbl = Assembler.newlabel

                    // Process LHS
                    processBoolExp (l, truelbl2, false, procOrModDecl)

                    // If false (go to end)
                    Assembler.emit ( BR (andendlbl))

                    // If true, process RHS
                    Assembler.mark (truelbl2)
                    processBoolExp (r, truelbl, false, procOrModDecl)

                    // Mark end-of-And
                    Assembler.mark (andendlbl)
                }

            // Or: Uses short-circuit evaluation
            case Or (l, r) =>

                if (negate) {
                    processBoolExp (And (Not (l), Not(r)), truelbl, false, procOrModDecl)
                }
                else {
                    // Process LHS
                    processBoolExp (l, truelbl, false, procOrModDecl)

                    // If false, process RHS
                    processBoolExp (r, truelbl, false, procOrModDecl)
                }

            // Other binary expressions
            case be : BinaryBoolExp => {
                val lreg = processNumExp (be.getLeft, procOrModDecl)
                val rreg = processNumExp (be.getRight, procOrModDecl)
                Assembler.emit (CMP (lreg, rreg))

                if (negate)
                    exp match {
                        case e : Equal => Assembler.emit (BNE (truelbl))
                        case ne : NotEqual => Assembler.emit (BEQ (truelbl))
                        case lt : LessThan => Assembler.emit (BGE (truelbl))
                        case lte : LessThanOrEqual => Assembler.emit (BGT (truelbl))
                        case gt : GreaterThan => Assembler.emit (BLE (truelbl))
                        case gte : GreaterThanOrEqual => Assembler.emit (BLT (truelbl))
                     }
                else
                    exp match {
                        case e : Equal => Assembler.emit (BEQ (truelbl))
                        case ne : NotEqual => Assembler.emit (BNE (truelbl))
                        case lt : LessThan => Assembler.emit (BLT (truelbl))
                        case lte : LessThanOrEqual => Assembler.emit (BLE (truelbl))
                        case gt : GreaterThan => Assembler.emit (BGT (truelbl))
                        case gte : GreaterThanOrEqual => Assembler.emit (BGE (truelbl))
                    }

                Assembler.freeReg (lreg)
                Assembler.freeReg (rreg)
            }
        }
    }

    /**
     * EncodeAssignment
     */
    def EncodeAssignment (targetdes : Desig, exp : Exp, procOrModDecl : Declaration) {

        // Process destination
        val tgtDesResult = processDesig (targetdes, procOrModDecl)

        // Process source
        exp match {

            // If source is a designator ...
            case srcdes : Desig => {
                // Get memory address as register and offset
                val srcDesResult = processDesig (srcdes, procOrModDecl)

                // Copy word-by-word
                val tempreg = Assembler.getFreeReg

                var i : Int = 0
                while (i < targetdes->objType->byteSize)
                {
                    Assembler.emit (LDW (tempreg, srcDesResult.regno, srcDesResult.offset + i))
                    Assembler.emit (STW (tempreg, tgtDesResult.regno, tgtDesResult.offset + i))
                    i = i + 4
                }

                Assembler.freeReg (tempreg)
            }

            // Other (single-valued) expressions
            case _ => {
                val srcReg = processNumExp (exp, procOrModDecl)

                // Make assignment
                Assembler.emit (STW (srcReg, tgtDesResult.regno, tgtDesResult.offset))

                Assembler.freeReg (srcReg)
            }
        }

        // Free registers
        if (tgtDesResult.regno != 0 && tgtDesResult.regno != 29)
            Assembler.freeReg (tgtDesResult.regno)
    }

    /**
     * EncodeIfStmt
     */
    def EncodeIfStmt (condexp : Exp, thenstmts : List[Statement], elsestmts : List[Statement],
                      procOrModDecl : Declaration) {

        // Process condition expression
        val truelbl = Assembler.newlabel

        processBoolExp (condexp, truelbl, false, procOrModDecl)

        // Action if false
        elsestmts.foreach (stmt => EncodeStatement (stmt, procOrModDecl))

        val exitlbl = Assembler.newlabel

        Assembler.emit (BR (exitlbl))

        // Action if true
        Assembler.mark (truelbl)

        thenstmts.foreach (stmt => EncodeStatement (stmt, procOrModDecl))

        Assembler.mark (exitlbl)
        
    }

    /**
     * EncodeWhileStmt
     */
    def EncodeWhileStmt (condexp : Exp, bodystmts : List[Statement], procOrModDecl : Declaration) {

        // Jump to test code
        val testlbl = Assembler.newlabel
        Assembler.emit (BR (testlbl))

        // Output loop statements
        val looplbl = Assembler.newlabel
        Assembler.mark (looplbl)
        bodystmts.foreach (stmt => EncodeStatement (stmt, procOrModDecl))

        // Loop test
        Assembler.mark (testlbl)
        processBoolExp (condexp, looplbl, false, procOrModDecl)
        
    }

    /**
     * EncodeWrite
     */
    def EncodeWrite (exp : Exp, procOrModDecl : Declaration) {

        // Process expression
        val reg = processNumExp (exp, procOrModDecl)

        // Call WRD
        Assembler.emit (WRD (reg))

        Assembler.freeReg (reg)
        
    }

    /**
     * EncodeWriteLn
     */
    def EncodeWriteLn (exp : Exp, procOrModDecl : Declaration) {

        // Process expression
        EncodeWrite (exp, procOrModDecl)

        // Call WRL
        Assembler.emit (WRL)
        
    }

    /**
     * EncodeRead
     */
    def EncodeRead (exp : Exp, procOrModDecl : Declaration) {

        // Process destination
        exp match {
            case des : Desig => {

                // Get a register to put the result of the read
                val reg = Assembler.getFreeReg

                // Read a value
                Assembler.emit (RD (reg))

                val desResult = processDesig (des, procOrModDecl)

                // Make assignment
                Assembler.emit (STW (reg, desResult.regno, desResult.offset))

                // Free registers
                if (desResult.regno != 0 && desResult.regno != 29)
                    Assembler.freeReg (desResult.regno)

                Assembler.freeReg (reg)
            }
        }
    }

    /**
     * Encode procedure actual parameters
     */
    def ProcessActualParams (fps : List[Declaration], aps : List[Exp], procOrModDecl : Declaration) {

        if (!fps.isEmpty) {

            val fp = fps.head
            val ap = aps.head

            // Note:  Can't just push the parameters onto the stack because
            // this would break the logic in EncodeProc()
            fp match {

                // RefVarDecls
                case rvd : RefVarDecl => {

                    ap match {
                        case des : Desig => {
                            val desResult = processDesig (des, procOrModDecl)

                            // Store parameter address
                            var reg : Byte = 0

                            // If no register allocated, allocate one
                            if (desResult.regno == 0 || desResult.regno >= 27) {
                                reg = Assembler.getFreeReg
                            }
                            // Otherwise use the same reg returned from processDesig
                            else {
                                reg = desResult.regno
                            }

                            Assembler.emit ( ADDI (reg, desResult.regno, desResult.offset))

                            // The +8 here is to skip over the backups of LNK and the old FP
                            Assembler.emit ( STW (reg, 30, fp.byteOffset + 8))

                            // Free registers
                            Assembler.freeReg (reg)
                        }
                    }
                }

                // Value parameters
                case _ => {

                    // Process source
                    ap match {

                        // If source is a designator ...
                        case srcdes : Desig => {

                            // Get memory address as register and offset
                            val srcDesResult = processDesig (srcdes, procOrModDecl)

                            // Copy word-by-word
                            val tempreg = Assembler.getFreeReg

                            var i : Int = 0
                            while (i < fp->objType->byteSize)
                            {
                                Assembler.emit (LDW (tempreg, srcDesResult.regno, srcDesResult.offset + i))

                                // The +8 is as above
                                Assembler.emit (STW (tempreg, 30, fp.byteOffset + 8 + i))
                                i = i + 4
                            }

                            Assembler.freeReg (tempreg)
                        }

                        // Other (single-valued) expressions
                        case _ => {

                            val srcReg = processNumExp (ap, procOrModDecl)

                            // The +8 is as above
                            Assembler.emit (STW (srcReg, 30, fp.byteOffset + 8))

                            Assembler.freeReg (srcReg)
                        }
                    }

                    val reg = processNumExp (ap, procOrModDecl)

                    Assembler.freeReg (reg)
                }
            }

            ProcessActualParams (fps.tail, aps.tail, procOrModDecl)
        }
    }

    /**
     * EncodeProcedureCall
     */
    def EncodeProcedureCall (desig: Exp, aps: List[Exp], procOrModDecl : Declaration) {

        desig match {

            case Ident ("Write")   => EncodeWrite (aps.head, procOrModDecl)
            case Ident ("WriteLn") => EncodeWriteLn (aps.head, procOrModDecl)
            case Ident ("Read")    => EncodeRead (aps.head, procOrModDecl)

            case id : Ident => {
                val pd = (id->decl).asInstanceOf[ProcDecl]

                // Set the static link
                // If the proc being called is a child of the current proc,
                // set the static link to the current proc's FP
                if (pd.parent == procOrModDecl) {
                    Assembler.emit (PSH (29, 30, 4))
                }
                // Otherwise the proc being called must be a sibling proc.
                // In this case, set the static link to the parent proc's static link
                else {
                    val reg = Assembler.getFreeReg

                    // Load the current static link
                    Assembler.emit (LDW (reg, 29, -12))

                    // Put it on the stack
                    Assembler.emit (PSH (reg, 30, 4))

                    Assembler.freeReg (reg)
                }

                // Process the actual parameters
                ProcessActualParams (pd.fps, aps, procOrModDecl)

                Assembler.emit (BSR (pd.label))
            }
            
            case _ => ()
        }
    }
}
