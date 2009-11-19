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

/**
 * Instruction set architecture for a simple RISC machine.
 */
object RISCISA {

    /**
     * A code sequence.
     */
    type Code = Seq[Instr]

    /**
     * Register numbers (0-31).  Program counter is R28.
     */
    type RegNo = Byte

    /**
     * Machine instructions.
     */
    abstract class Instr

    /**
     * Shift the value in register c by b and store the result in register a.
     */
    case class MOV (a : RegNo, b : Int, c : RegNo) extends Instr

    /**
     * Shift the value im by b and store the result in register a.
     */
    case class MOVI (a : RegNo, b : RegNo, im : Int) extends Instr

    /**
     * Shift the value in register c by b and store the negation of the result
     * in register a.
     */
    case class MVN (a : RegNo, b : RegNo, c : RegNo) extends Instr

    /**
     * Shift the value im by b and store the negation of the result in
     * register a.
     */
    case class MVNI (a : RegNo, b : RegNo, im : Int) extends Instr

    /**
     * Add the values in registers b and c, store the result in register a.
     */
    case class ADD (a : RegNo, b : RegNo, c : RegNo) extends Instr

    /**
     * Add the value in register b and the value im, store the result
     * in register a.
     */
    case class ADDI (a : RegNo, b : RegNo, im : Int) extends Instr

    /**
     * Subtract the values in registers b and c, store the result in
     * register a.
     */
    case class SUB (a : RegNo, b : RegNo, c : RegNo) extends Instr

    /**
     * Subtract the value in register b and the value im, store the result
     * in register a.
     */
    case class SUBI (a : RegNo, b : RegNo, im : Int) extends Instr

    /**
     * Muliply the values in registers b and c, store the result in register a.
     */
    case class MUL (a : RegNo, b : RegNo, c : RegNo) extends Instr

    /**
     * Multiply the value in register b and the value im, store the result
     * in register a.
     */
    case class MULI (a : RegNo, b : RegNo, im : Int) extends Instr

    /**
     * Divide the values in registers b and c, store the (integer) result in
     * register a.
     */
    case class DIV (a : RegNo, b : RegNo, c : RegNo) extends Instr

    /**
     * Divide the value in register b and the value im, store the (integer)
     * result in register a.
     */
    case class DIVI (a : RegNo, b : RegNo, im : Int) extends Instr

    /**
     * Divide the values in registers b and c, store the (integer) remainder
     * in register a.
     */
    case class MOD (a : RegNo, b : RegNo, c : RegNo) extends Instr

    /**
     * Divide the value in registers b and the value im, store the (integer)
     * remainder in register a.
     */
    case class MODI (a : RegNo, b : RegNo, im : Int) extends Instr

    /**
     * Set the Z condition code if the contents of registers b and c
     * are equal.  Set the N condition code if the content of register
     * b is less than the content of register c.
     */
    case class CMP (b : RegNo, c : RegNo) extends Instr

    /**
     * Set the Z condition code if the content of register b and the
     * value im are equal, otherwise clear Z.  Set the N condition code
     * if the content of register b is less than the value im, otherwise
     * clear N.
     */
    case class CMPI (b : RegNo, im : Int) extends Instr

    /**
     * If register a contains a value that is negative or greater than
     * or equal to the value im, set register a to zero.
     */
    case class CHKI (a : RegNo, im : Int) extends Instr

    /**
     * Load register a with the word value stored in memory at the
     * address given by the contents of register b plus the value im.
     * The lowest two bits of the address are ignored.
     */
    case class LDW (a : RegNo, b : RegNo, im : Int) extends Instr

    /**
     * Load register a with the byte value stored in memory at the
     * address given by the contents of register b plus the value im.
     */
    case class LDB (a : RegNo, b : RegNo, im : Int) extends Instr

    /**
     * Load register a with the word value stored in register b.  The
     * lowest two bits of the address are ignored. Subtract (???) im from
     * the contents of register b and store the result in register b.
     */
    case class POP (a : RegNo, b : RegNo, im : Int) extends Instr

    /**
     * Store the contents of register a to memory at the address given
     * by the contents of register b plus the value im.  The lowest two
     * bits of the address are ignored.
     */
    case class STW (a : RegNo, b : RegNo, im : Int) extends Instr

    /**
     * Store the least-significant byte of the contents of register a to
     * memory at the address given by the contents of register b plus the
     * value im.
     */
    case class STB (a : RegNo, b : RegNo, im : Int) extends Instr

    /**
     * Add im to the contents of register b and store the
     * result in register b.  Store the value in register a into
     * memory at the address given by the contents of register b.
     * The lowest two bits of the address are ignored.
     */
    case class PSH (a : RegNo, b : RegNo, im : Int) extends Instr

    /**
     * Read an integer variable from standard input and store the value
     * in register a.
     */
    case class RD (a : RegNo) extends Instr

    /**
     * Write a decimal representation of the value in register c to
     * standard output.
     */
    case class WRD (c : RegNo) extends Instr

    /**
     * Write a hexadecimal representation of the value in register c to
     * standard output.
     */
    case class WRH (c : RegNo) extends Instr

    /**
     * Write a newline to standard output.
     */
    case object WRL extends Instr

    /**
     * Abstract interface for all branch instructions.  Branches are
     * created using symbolic labels. The assembler sets the disp
     * field once the symbolic label has been resolved to an offset.
     */
    abstract class Branch extends Instr {
        val label : Int
        var disp : Int = _
        override def toString = {super.toString + " (" + disp + ")"}
    }

    /**
     * If the Z condition code is set, set the program counter to its
     * value plus four times disp.
     */
    case class BEQ (val label : Int) extends Branch

    /**
     * If the Z condition code is clear, set the program counter to its
     * value plus four times disp.
     */
    case class BNE (val label : Int) extends Branch

    /**
     * If the N condition code is set, set the program counter to its
     * value plus four times disp.
     */
    case class BLT (val label : Int) extends Branch

    /**
     * If the N condition code is clear, set the program counter to its
     * value plus four times disp.
     */
    case class BGE (val label : Int) extends Branch

    /**
     * If either of the Z or N condition codes is set, set the program
     * counter to its value plus four times disp.
     */
    case class BLE (val label : Int) extends Branch

    /**
     * If both of the Z and N condition codes are clear, set the program
     * counter to its value plus four times disp.
     */
    case class BGT (val label : Int) extends Branch

    /**
     * Set the program counter to its value plus disp.
     */
    case class BR (val label : Int) extends Branch

    /**
     * Set R31 to the value of the program counter plus one. Set the
     * program counter to its value plus disp.
     */
    case class BSR (val label : Int) extends Branch

    /**
     * Set the program counter to the value in register c.  If that
     * value is zero, halt the machine.
     */
    case class RET (c : Int) extends Instr

}
