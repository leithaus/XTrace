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

package kiama.attribution

object DynamicAttribution extends DynamicAttribution

/**
 * Support for dynamic attribution of syntax trees.
 * Dynamic attributes definitions can be extended at runtime.
 *
 * @author Lennart Kats <lennart add lclnet.nl>
 * @author Tony Sloane <Anthony.Sloane add mq.edu.au>
 */
trait DynamicAttribution extends AttributionBase {

    import scala.collection.mutable._
    import scala.collection.jcl.IdentityHashMap

    type ChangeBuffer = ArrayBuffer[(DynamicAttribute[_, _], _ ==> _)]

    private var currentRecordedChanges : ChangeBuffer = null
    private val allRecordedChanges = new IdentityHashMap[AnyRef, ChangeBuffer]
    private var equationsVersion = 0

    /**
     * Lazily resets all memoisation tables.
     */
    def resetMemo = equationsVersion += 1

    /**
     * Define an attribute of T nodes of type U by the function f.
     */
    def attr[T <: AnyRef,U] (f : T ==> U) : T ==> U =
        new DynamicAttribute (f)

    /**
     * Define an attribute of T nodes of type U by the function f,
     * which takes the current node and its parent as its arguments.
     * T must be Attributable so that parents can be accessed.
     */
    def childAttr[T <: Attributable,U] (f : T => Attributable ==> U) : T ==> U = {
        val childF = new PartialFunction[T,U] {
            def apply (t : T) = f (t) (t.parent)
            def isDefinedAt (t : T) = f (t) isDefinedAt t.parent
        }
        attr (childF)
    }

    /**
     * Implicitly converts partial functions to support the + operator.
     **/
    implicit def internalToDynamicAttribute[T <: AnyRef,U] (f : Function[T,U]) : DynamicAttribute[T,U] =
        f match {
            case f : DynamicAttribution#DynamicAttribute[_, _] => f.asInstanceOf[DynamicAttribute[T,U]]
            case f => throw new UnsupportedOperationException("Cannot only add partial functions to existing attributes")
        }

    /**
     * Defines a new scope in which a dynamic attribution module is active.
     * At the end of the scope, the module is unloaded again.
     *
     * @param attributeInitializer  A module defining dynamic attributes.
     * @param block                 A block to evaluate.
     */
    def using[T] (attributeInitializer : => AnyRef) (block : => T) = {
        try {
            use (attributeInitializer)
            block
        } finally {
            endUse (attributeInitializer)
        }
    }

    /**
     * Activates a module that defines dynamic attributes,
     * allowing it to be deactivated again using {@link #endUse}.
     */
    def use[T] (attributeInitializer : => AnyRef) {
        val prevRecordedChanges = currentRecordedChanges
        try {
            currentRecordedChanges = new ArrayBuffer
            val recordedChanges = currentRecordedChanges

            val initialized = attributeInitializer // import initializer
            currentRecordedChanges = null

            if (allRecordedChanges contains initialized) reuse (initialized)
        } finally {
           currentRecordedChanges = prevRecordedChanges
        }
    }

    private def reuse (attributeInitializer : AnyRef) {
        for ((attr, function) <- allRecordedChanges (attributeInitializer))
            attr += function
    }

    /**
     * Dectivates a module that defines dynamic attributes,
     * activated using {@link #use}.
     */
    def endUse (attributeInitializer : AnyRef) {
        for ((attr, function) <- allRecordedChanges (attributeInitializer))
            attr -= function
    }

    class DynamicAttribute[T,U] (private var f : T ==> U) extends (T ==> U) {
        private val memo = new java.util.IdentityHashMap[T, Option[U]]
        private var memoVersion = equationsVersion

        def apply (t : T) = {
            if (memoVersion != equationsVersion) {
                memoVersion = equationsVersion
                memo.clear
            }

            memo.get (t) match {
                case Some (u) => u
                case None    => throw new IllegalStateException("Cycle detected in attribute evaluation")
                case null =>
                    memo.put (t, None)
                    val result = f (t)
                    memo.put (t, Some (result))
                    result
            }
        }

        def isDefinedAt (t : T) = f isDefinedAt t

        def composedF : ComposedPartialFunction[T,U] =
            f match {
                case _ : ComposedPartialFunction[_,_] => f.asInstanceOf[ComposedPartialFunction[T,U]]
                case _ : PartialFunction[_,_]         => val g = new ComposedPartialFunction(f); f = g; g
            }

        def += (g : T ==> U) {
            val uncached : T ==> U = g match {
                case g : DynamicAttribute[_, _] => g.f
                case _                          => g
            }

            if (currentRecordedChanges != null) currentRecordedChanges += (this, uncached)
            composedF += uncached
            resetMemo
        }

        def -= (g : T ==> U) {
            composedF -= g
            resetMemo
        }
    }

    /**
     * A partial function composed of an ordered, mutable buffer of
     * PartialFunction instances.
     */
    class ComposedPartialFunction[T,U] (f : T ==> U) extends (T ==> U) {
        val functions = new ArrayBuffer[T ==> U]

        def isDefinedAt (i : T) = functions.exists(_ isDefinedAt i)

        def apply (t : T) : U = {
            for (i <- (functions.size - 1) until (-1, -1)) {
                if (functions(i) isDefinedAt t) return functions(i)(t)
            }
            throw new MatchError("Function not defined for " + t)
        }

        def += (g : T ==> U) {
            functions += g
        }

        def -= (g : T ==> U) {
            val removed = functions.lastIndexOf(f)
            functions.remove(removed)
        }

        this += f
    }
}
