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

package kiama.machine

/**
 * A deterministic abstract state machine defined by its main rule and
 * called name.
 */
abstract class Machine (val name : String) {

    /**
     * Debug flag. Set this to true in sub-classes or objects to obtain
     * tracing information during execution of the machine.
     */
    def debug = false

    /**
     * An item of abstract state machine state holding a value of type T
     * and called sname.
     */
    case class State[T] (sname : String) {

        /**
         * The value of this item of state.  None means undefined.
         */
        private var _value : Option[T] = None

        /**
         * Is this state item undefined or not?
         */
        def isUndefined : Boolean = _value isEmpty

        /**
         * Make this state item undefined.
         */
        def undefine =
            _value = None

        /**
         * Return the value of this state item if it's defined.  Otherwise
         * abort execution.
         */
        def value : T =
            _value match {
                case None     => error ("State.value: " + name + "." + sname + " is undefined")
                case Some (t) => t
            }

        /**
         * Update this item of state to the value t.  The update is actually
         * delayed until the end of the step when all updates in that
         * step happen simultaneously (along with consistency checking).  The
         * state value only becomes defined when this latter process happens.
         */
        def := (t : T) {
            updates = Update (this, t) :: updates
            if (debug)
                println (name + " new update: " + sname + " := " + t)
        }

        /**
         * Update this item of state to the value t.  The update occurs
         * immediately.
         */
        def update (t : T) =
            _value = Some (t)

        /**
         * Make a printable representation.
         */
        override def toString : String =
            sname + " = " +
                (_value match {
                    case None     => "undef"
                    case Some (t) => t
                 })

    }

    /**
     * Implicitly allow a state value of type T to be used as a value of type T.
     */
    implicit def stateTToT[T] (t : State[T]) = t.value

    /**
     * An update of an item of state s to have the value t.
     */
    case class Update[T] (s : State[T], t : T) {

        /**
         * Perform this update.
         */
        def perform {
            s.update (t)
        }

    }

    /**
     * The updates for the current step of execution of this machine.
     */
    private var updates : List[Update[_]] = _

    /**
     * Initialise the state of this machine.  This routine is called
     * before the first step of the machine is attempted.
     */
    def init

    /**
     * The rule to execute to run one step of this machine.
     */
    def main

    /**
     * Perform a step of this machine.  Return true if some updates were
     * made or false if none.
     */
    def step : Boolean = {
        updates = Nil
        main
        if (updates isEmpty)
            false
        else {
            // FIXME: check updates for consistency
            updates.map (_.perform)
            true
        }
    }

    /**
     * Execute the steps of this machine.  Halt when a step makes no
     * updates.  init should be called before this method.
     */
    def steps = {
        var nsteps = 0
        do {
            if (debug) {
                println (name + " step " + nsteps)
                nsteps += 1
            }
        } while (step)
    }

    /**
     * Run this machine by initialising its state and then executing
     * its steps.
     */
    def run = {
        init
        steps
    }

}
