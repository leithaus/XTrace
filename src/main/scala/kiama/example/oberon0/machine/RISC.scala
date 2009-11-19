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

package kiama.example.oberon0.machine

import RISCISA._
import kiama.machine.Machine

/**
 * Abstract state machine simulation of a simple RISC architecture.
 */
class RISC (code : Code) extends Machine ("RISC") {

    /**
     * Debug flag. Set this to true in sub-classes or objects to obtain
     * tracing information during execution of the machine.
     */
    override def debug = false

    /**
     * Words are 32-bits long.
     */
    type Word = Int

    /**
     * A named register.
     */
    case class Reg (name : String) extends State[Int] (name)

    /**
     * Integer register file addressed by 0-31.
     */
    lazy val R = Array.fromFunction (i => Reg ("R" + i.toString)) (32)

    /**
     * The program counter is register 28.
     */
    lazy val PC = R (28)
    lazy val FP = R (29)
    lazy val SP = R (30)
    lazy val LNK = R (31)

    /**
     * Byte addressed store of words.
     */
    val Mem = State[Map[Int,Word]] ("Mem")

    /**
     * Condition code: zero.
     */
    val Z = State[Boolean] ("Z")

    /**
     * Condition code: less than.
     */
    val N = State[Boolean] ("N")

    /**
     * Halt flag.  Undefined until the machine is to stop executing.
     */
    val halt = State[String] ("halt")

    /**
     * Initialise the machine.
     */
    def init {
        Mem.update (Map ())
        PC.update (0)
        R (0).update(0)		// Set R0 = 0
        Z.update (false)
        N.update (false)
        halt.undefine
    }

    /**
     * The main rule of this machine.
     */
    def main =
        if (halt isUndefined)
            execute (code (PC))

    /**
     * Execute a single instruction.
     */
    def execute (instr : Instr) = {
        if (debug)
            println (name + " exec: " + instr)
        arithmetic (instr)
        memory (instr)
        control (instr)
        inputoutput (instr)
    }

    /**
     * Execute arithmetic instructions.
     */
    def arithmetic (instr : Instr) =
        instr match {
            case MOV (a, b, c)   => R (a) := R (c) << b
            case MOVI (a, b, im) => R (a) := im << b
            case MVN (a, b, c)   => R (a) := - (R (c) << b)
            case MVNI (a, b, im) => R (a) := - (im << b)
            case ADD (a, b, c)   => R (a) := R (b) + (R (c) : Int)
            case ADDI (a, b, im) => R (a) := R (b) + im
            case SUB (a, b, c)   => R (a) := R (b) - (R (c) : Int)
            case SUBI (a, b, im) => R (a) := R (b) - im
            case MUL (a, b, c)   => R (a) := R (b) * (R (c) : Int)
            case MULI (a, b, im) => R (a) := R (b) * im
            case DIV (a, b, c)   => R (a) := R (b) / (R (c) : Int)
            case DIVI (a, b, im) => R (a) := R (b) / im
            case MOD (a, b, c)   => R (a) := R (b) % (R (c) : Int)
            case MODI (a, b, im) => R (a) := R (b) % im
            case CMP (b, c)      => Z := R (b).value == R (c).value
                                    N := R (b).value < (R (c).value : Int)
            case CMPI (b, im)    => Z := R (b).value == im
                                    N := R (b).value < im
            case CHKI (a, im)    => if ((R (a) < 0) || (R (a) >= im))
                                        R (a) := 0
            case _ => ()
        }

    /**
     * Execute memory instructions.
     */
    def memory (instr : Instr) =
        try {
            instr match {
                case LDW (a, b, im) => R (a) := Mem ((R (b) + im) / 4)
                case LDB (a, b, im) => halt := "LDB not implemented"
                case POP (a, b, im) => R (a) := Mem ((R (b) - im) / 4)
                                       R (b) := R (b) - im
                case STW (a, b, im) => Mem := Mem + (((R (b) + im) / 4, R (a)))
                case STB (a, b, im) => halt := "STB not implemented"
                case PSH (a, b, im) => Mem := Mem + ((R (b) / 4, R (a)))
                                       R (b) := R (b) + im
                case _ => ()
            }
        }
        catch {
            case _ => println ("XXX Exception at " + instr)
            println ("Mem = " + Mem)
            halt := "Halt"
        }

    /**
     * Execute control instructions, including default control step.
     */
    def control (instr : Instr) =
        instr match {
            case b : BEQ if (Z.value) => PC := PC + b.disp
            case b : BNE if (!Z.value) => PC := PC + b.disp
            case b : BLT if (N.value) => PC := PC + b.disp
            case b : BGE if (!N.value) => PC := PC + b.disp
            case b : BLE if (Z.value || N.value) => PC := PC + b.disp
            case b : BGT if (!Z.value && !N.value) => PC := PC + b.disp
            case b : BR => PC := PC + b.disp
            case b : BSR => R (31) := PC + 1
                            PC := PC + b.disp

            case RET (c) => PC := R (c)
                            if (R (c).value == 0) halt := "Halt"
            case _  => PC := PC + 1
        }

    /**
     * Execute input/output instructions.
     */
    def inputoutput (instr : Instr) =
        instr match {
            case RD (a)  => R (a) := readInt
            case WRD (c) => print (R (c).value)
            case WRH (c) => print (R (c) toHexString)
            case WRL     => println
            case _       =>
        }

}
