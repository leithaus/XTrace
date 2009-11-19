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

package kiama.example.oberon0

import compiler.Parser

/**
 * Main program for the Oberon0 implementation.  Parses and checks
 * the Oberon0 program named on the command line.  If checking passes,
 * then encodes the program as RISC assembler and runs the assembler
 * using a machine simulator.
 */
object Main extends Parser
{
    import compiler.AST._
    import compiler.ErrorCheck.collectErrors
    import compiler.Encoder.EncodeModule
    import assembler.Assembler
    import machine.RISC
    import java.io.FileReader
    import java.io.FileNotFoundException
    import kiama.util.Messaging._

    def main (args: Array[String])
    {
        // println ("Input : " + args(0))

        var result : ParseResult[ModuleDecl] = null

        // PARSING
        try {
            val fr : FileReader = new FileReader(args(0))
            result = parseAll(moduledecl, fr)
        }
        catch {
            case exc : FileNotFoundException => println (exc.getMessage)
            return
        }

        result match {
            case Success (mod, in) => {

                // val buffer = new StringBuilder
                // mod.pretty(buffer, 0)
                // println ("Successful parse:")
                // println (buffer.toString)
                // println ("Position = " + in.pos)

                // SEMANTIC ANALYSIS
                mod->collectErrors

                if (messagecount == 0) {
                    // println ("\nNo semantic errors")

                    // Encode
                    EncodeModule (mod)

                    // Assemble
                    val instrs = Assembler.getcode
                    // println ("\nInstructions: ")
                    // instrs.foreach (instr => println (instr))

                    // Run
                    // println ("\nProgram output: ")
                    val mymachine = new RISC (instrs)
                    mymachine.init
                    mymachine.steps
                }
                else {
                    // println ("\nSemantic errors occurred:")
                    report
                }
            }

            case f : Failure => println (f)
        }
    }
}