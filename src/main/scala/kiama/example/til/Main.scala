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

package kiama.example.til

import java.io.Reader
import kiama.parsing.CharPackratParsers
import kiama.rewriting.Rewriter

/**
 * Standard main program for TIL chairmarks.  Also includes a simple
 * testing framework.
 */
trait Main {

    import java.io.{CharArrayReader,FileNotFoundException,FileReader}
    import junit.framework.Assert._

    /**
     * Accept file name arguments and process them one-by-one by
     * passing a reader on the file to process.  The resulting
     * value is printed.
     */
    def main (args : Array[String]) {
        for (arg <- args) {
            try {
                val reader = new FileReader (arg)
                val result = process (reader)
                println (result)
            } catch {
                case e : FileNotFoundException =>
                    println ("can't open " + arg + " for reading")
            }
        }
    }

    /**
      * Process the file given by the argument reader and return
      * some useful result.
      */
    def process (reader : Reader) : Any

    /**
     * Try to process a string and expect a given result.  Return a
     * JUnit test case result.
     */
    def test[T] (str : String, result : T) {
        val r = process (new CharArrayReader (str.toArray))
        if (r != result)
            fail ("got : " + r.toString + ", expected : " + result)
    }

}

/**
 * Standard main program for TIL chairmarks that parse.
 */
trait ParsingMain extends Main with CharPackratParsers {

    /**
      * Process the file given by the argument reader by parsing it
      * and returning the resulting AST.
      */
    def process (reader : Reader) : Any =
        parseAll (parse, reader) match {
            case Success (p, _) => p
            case f : Failure    => f
        }

    /**
     * The root type of the AST being processed.
     */
    type Root

    /**
     * Parse a file, returning an AST.
     */
    def parse : Parser[Root]

}

/**
 * Standard main program for TIL chairmarks that parse and transform.
 */
trait TransformingMain extends ParsingMain with Rewriter {

    /**
     * The root type of the AST being processed. Needs to be sub-type
     * of Term so that we can transform it using rewriting.
     */
    type Root <: Term

    /**
      * Process the file given by the argument reader by parsing it,
      * transforming it and returning the resulting AST.
      */
    override def process (reader : Reader) : Any =
        parseAll (parse, reader) match {
            case Success (p, _) => transform (p)
            case f : Failure    => f
        }

    /**
     * Transform a single AST.
     */
    def transform (ast : Root) : Root

}
