/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009 Anthony M Sloane, Macquarie University.
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

/**
 * Facility for buffering of messages associated with positioned values.
 */
object Messaging {

    import scala.collection.mutable.ListBuffer
    import scala.util.parsing.input.Positional
    import scala.util.parsing.input.Position

    /**
     * A message record.
     */
    case class Record (pos : Position, message : String)

    /**
     * Buffer of messages.
     */
    var messages = new ListBuffer[Record] ()

    /**
     * Buffer a new message associated with the given positioned value.
     */
    def message (value : Positional, message : String) =
        messages += Record (value.pos, message)

    /**
     * Return the number of messages that are buffered.
     */
    def messagecount : Int =
        messages.size

    /**
     * Output the messages that have been buffered in order of position.
     */
    def report =
        for (m <- messages.toList.sort (_.pos < _.pos)) {
            print (m.pos.line)
            print ('.')
            print (m.pos.column)
            print (": ")
            println (m.message)
        }

    /**
     * Reset the message buffer to empty.
     */
    def resetmessages = {
        messages = new ListBuffer[Record] ()
    }

}
