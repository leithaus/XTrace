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

package kiama.util

import jline.ConsoleReader
import kiama.parsing.CharPackratParsers
import org.scalacheck._

/**
 * General support for applications that implement read-eval-print loops (REPLs).
 */
trait REPL {

    /**
     * Read lines from standard input and pass non-null ones to processline.
     * Continue until processline returns false. The command-line arguments
     * are ignored.  Calls setup before entering the loop and prompt
     * each time input is about to be read.
     */
    def main (args : Array[String]) {
        setup
        val reader = new ConsoleReader ()
        while (true) {
            val line = reader.readLine (prompt)
            if (line == null)
                return
            else
                processline (line)
        }
    }

    /**
     * Carry out setup processing for the REPL.  Default: do nothing.
     */
    def setup { }

    /**
     * Define the prompt (default: "> ").
     */
    def prompt = "> "

    /**
     * Process a user input line.
     */
    def processline (line : String)

}

/**
 * A REPL that parses its input lines into a value (such as an abstract syntax
 * tree), then processes them.
 */
trait ParsingREPL[T] extends REPL with CharPackratParsers {

    import scala.util.parsing.input.CharSequenceReader

    /**
     * Process a user input line by parsing it to get a value of type T,
     * then passing it to the type-specific process.
     */
    def processline (line : String) {
        val in = new CharSequenceReader (line)
        parse (in) match {
            case Success (e, in) if in.atEnd =>
                process (e)
            case Success (_, in) =>
                println ("extraneous input at " + in.pos)
            case f @ Failure (_, _) =>
                println (f)
        }
    }

    /**
     * The parser to use to convert user input lines into values.
     */
    def parse : Parser[T]

    /**
     * Process a user input value.
     */
    def process (t : T)

}

/**
 * A REPL that uses ScalaCheck to generate random instances of abstract
 * syntax trees of type T and prints them.
 */
trait GeneratingREPL[T] extends REPL {

    /**
     * Carry out setup processing for the REPL.
     */
    override def setup {
        println ("Each time you hit ENTER a new instance is generated and printed.")
    }

    /**
     * Display a prompt.
     */
    override def prompt = "Hit ENTER to generate an instance: "

    /**
     * The generator to use to make values of type T.
     */
    def generator : Arbitrary[T]

    /**
     * Generate a new instance and print it, ignoring the input line.
     */
    def processline (line : String) {
        generator.arbitrary (Gen.defaultParams) match {
            case Some (t) => process (t)
            case None     => println ("can't generate an instance")
        }
    }

    /**
     * Process a generated value.  Default: print it.
     */
    def process (t : T) {
        println (t)
    }

}
