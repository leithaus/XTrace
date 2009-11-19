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

/**
 * Base support for attribution of syntax trees in a functional style.
 * Includes circular attributes but needs to be augmented with basic attributes
 * and parameterised attributes.
 */
trait AttributionBase {

    /**
     * Convenient type constructor for partial functions.
     */
    type ==>[T,U] = PartialFunction[T,U]

    /**
     * Global state for the circular attribute evaluation algorithm
     * and the memoisation tables.
     */
    private object CircularState {
        var IN_CIRCLE = false
        var CHANGE = false
    }

    /**
     * An attribute of a node type T with value of type U which has a circular
     * definition.  The value of the attribute is computed by the function f
     * which may itself use the value of the attribute.  init specifies an
     * initial value for the attribute.  The attribute (and any circular attributes
     * on which it depends) are evaluated until no value changes (i.e., a fixed
     * point is reached).  The final result is memoised so that subsequent evaluations
     * return the same value.
     *
     * This code implements the basic circular evaluation algorithm from "Circular
     * Reference Attributed Grammars - their Evaluation and Applications", by Magnusson
     * and Hedin from LDTA 2003.
     */
    class CircularAttribute[T <: AnyRef,U] (init : U, f : T ==> U) extends (T ==> U) {

        /**
         * Has the value of this attribute for a given tree already been computed?
         */
        private val computed = new scala.collection.jcl.IdentityHashMap[T,Unit]

        /**
         * Has the attribute for given tree been computed on this iteration of the
         * circular evaluation?
         */
        private val visited = new scala.collection.jcl.IdentityHashMap[T,Unit]

        /**
         * The memo table for this attribute.
         */
        private val memo = new scala.collection.jcl.IdentityHashMap[T,U]

        /**
         * Return the value of the attribute for tree t, or the initial value if
         * no value for t has been computed.
         */
        private def value (t : T) : U =
            memo.get (t) match {
                case Some (u) => u
                case None     => init
            }

        /**
         * Return the value of this attribute for node t.  Essentially Figure 6
         * from the CRAG paper.
         */
        def apply (t : T) : U = {
            if (computed contains t) {
                value (t)
            } else if (!CircularState.IN_CIRCLE) {
                CircularState.IN_CIRCLE = true
                visited (t) = ()
                var u = init
                do {
                    CircularState.CHANGE = false
                    val newu = f (t)
                    if (u != newu) {
                        CircularState.CHANGE = true
                        u = newu
                    }
                } while (CircularState.CHANGE)
                visited -= t
                computed (t) = ()
                memo (t) = u
                CircularState.IN_CIRCLE = false
                u
            } else if (! (visited contains t)) {
                visited (t) = ()
                var u = value (t)
                val newu = f (t)
                if (u != newu) {
                    CircularState.CHANGE = true
                    u = newu
                    memo (t) = u
                }
                visited -= t
                u
            } else
                value (t)
        }

        /**
         * A circular attribute is defined at the same places as its
         * defining function.
         */
        def isDefinedAt (t : T) =
            f isDefinedAt t

    }

    /**
     * Support for parameterised attributes: argument, node pair comparison.
     */
    protected class ParamAttributeKey (val arg : Any, val node : AnyRef) {
        override def equals(o : Any) =
            o match {
                case o : ParamAttributeKey =>
                  arg == o.arg &&                                        // object equality
                  (if (node eq null) o.node eq null else node eq o.node) // reference equality
                case _ => false
            }

        override val hashCode = System.identityHashCode(node) ^ arg.hashCode
    }

    /**
     * Define a circular attribute of T nodes of type U by the function f.
     * f is allowed to depend on the value of this attribute, which will be
     * given by init initially and will be evaluated iteratively until a
     * fixed point is reached (in conjunction with other circular attributes
     * on which it depends).  The final value is cached.
     */
    def circular[T <: AnyRef,U] (init : U) (f : T ==> U) : T ==> U =
        new CircularAttribute (init, f)

    /**
     * Define an attribute of T nodes of type U given by the constant value u.
     * u is evaluated at most once.
     */
    def constant[T <: AnyRef,U] (u : => U) : T ==> U =
        new (T ==> U) {
            lazy val result = u
            def apply (t : T) = result
            def isDefinedAt (t : T) = true
        }

}

/**
 * Attribution of syntax trees in a functional style with attribute values
 * cached so that each value is computed at most once.
 */
object Attribution extends Attribution

/**
 * Attribution of syntax trees in a functional style with attribute values
 * cached so that each value is computed at most once.
 */
trait Attribution extends AttributionBase {

    /**
     * Global state for the memoisation tables.
     */
    private object MemoState {
        var MEMO_VERSION = 0
    }

    /**
     * Lazily reset all memoisation tables.
     */
    def resetMemo = MemoState.MEMO_VERSION += 1

    /**
     * An attribute of a node type T with value of type U, supported by a memo
     * table and circularity test.  The value of the attribute is computed by
     * the function f.  The result is memoised so that it is only evaluated once.
     * f should not itself require the value of this attribute. If it does, a
     * circularity error is reported.
     */
    class CachedAttribute[T <: AnyRef,U] (f : T ==> U) extends (T ==> U) {

        /**
         * The memo table for this attribute, with <code>memo(t) == Some(v)</code>
         * representing the node t having the value v.  <code>memo(t) = None</code>
         * means that the attribute for t is currently being evaluated.  Note that
         * this needs to be some form of identity map so that value equal trees are
         * not treated as equal unless they are actually the same reference.
         */
        private val memo = new scala.collection.jcl.IdentityHashMap[T,Option[U]]
        private var memoVersion = MemoState.MEMO_VERSION

        /**
         * Return the value of this attribute for node t, raising an error if
         * it depends on itself.
         */
        def apply (t : T) : U = {
            if (memoVersion != MemoState.MEMO_VERSION) {
                memoVersion = MemoState.MEMO_VERSION
                memo.clear
            }
            memo.get (t) match {
                case Some (None)     => throw new IllegalStateException ("Cycle detected in attribute evaluation")
                case Some (Some (u)) => u
                case None =>
                    memo (t) = None
                    val u = f (t)
                    memo (t) = Some (u)
                    u
            }
        }

        /**
         * A cached attribute is defined at the same places as its
         * defining function.
         */
        def isDefinedAt (t : T) =
            f isDefinedAt t

    }

    /**
     * A variation of the CachedAttribute class for parameterised attributes.
     */
    class CachedParamAttribute[TArg,T <: AnyRef,U] (f : TArg => T ==> U)
            extends (TArg => T ==> U) {

        private val memo = new scala.collection.jcl.HashMap[ParamAttributeKey,Option[U]]
        private var memoVersion = MemoState.MEMO_VERSION

        /**
         * Return the value of this attribute for node t, raising an error if
         * it depends on itself.
         */
        def apply (arg : TArg) : T ==> U =
            new (T ==> U) {

                def apply (t : T) : U = {
                    if (memoVersion != MemoState.MEMO_VERSION) {
                        memoVersion = MemoState.MEMO_VERSION
                        memo.clear
                    }
                    val key = new ParamAttributeKey (arg, t)
                    memo.get (key) match {
                        case Some (None)     => throw new IllegalStateException ("Cycle detected in attribute evaluation")
                        case Some (Some (u)) => u
                        case None =>
                            memo (key) = None
                            val u = f (arg) (t)
                            memo (key) = Some (u)
                            u
                    }
                }

                def isDefinedAt (t : T) =
                    f (arg) isDefinedAt t

            }

    }

    /**
     * Define an attribute of T nodes of type U by the function f, which
     * should not depend on the value of this attribute.  The computed
     * attribute value is cached so it will be computed at most once.
     */
    def attr[T <: AnyRef,U] (f : T ==> U) : T ==> U =
        new CachedAttribute (f)

    /**
     * Define an attribute of T nodes of type U by the function f,
     * which takes an argument of type TArg.  The computed attribute value
     * for a given TArg is cached so it will be computed at most once.
     */
    def paramAttr[TArg,T <: AnyRef,U] (f : TArg => T ==> U) : TArg => T ==> U =
        new CachedParamAttribute (f)

    /**
     * Define an attribute of T nodes of type U by the function f,
     * which takes the current node and its parent as its arguments.
     * T must be Attributable so that parents can be accessed.
     */
    def childAttr[T <: Attributable,U] (f : T => Attributable ==> U) : T ==> U =
        attr { case t => f (t) (t.parent) }

}

/**
 * Attribution of syntax trees in a functional style with attribute values
 * computed each time they are accessed.
 */
object UncachedAttribution extends UncachedAttribution

/**
 * Attribution of syntax trees in a functional style with attribute values
 * computed each time they are accessed.
 */
trait UncachedAttribution extends AttributionBase {

    /**
     * An attribute of a node type T with value of type U, supported by a circularity
     * test.  The value of the attribute is computed by the function f.  f will be
     * called each time the value of the attribute is accessed.  f should not itself
     * require the value of this attribute. If it does, a circularity error is reported.
     */
    class UncachedAttribute[T <: AnyRef,U] (f : T ==> U) extends (T ==> U) {

        /**
         * Are we currently evaluating this attribute for a given tree?
         */
        private val visited = new scala.collection.jcl.IdentityHashMap[T,Unit]

        /**
         * Return the value of this attribute for node t, raising an error if
         * it depends on itself.
         */
        def apply (t : T) : U = {
            if (visited contains t) {
                throw new IllegalStateException ("Cycle detected in attribute evaluation")
            } else {
                visited (t) = ()
                val u = f (t)
                visited -= t
                u
            }
        }

        /**
         * An uncached attribute is defined at the same places as its
         * defining function.
         */
        def isDefinedAt (t : T) =
            f isDefinedAt t

    }

    /**
     * A variation of the UncachedAttribute class for parameterised attributes.
     */
    class UncachedParamAttribute[TArg,T <: AnyRef,U] (f : TArg => T ==> U) extends (TArg => T ==> U) {

        /**
         * Are we currently evaluating this attribute for a given argument and tree?
         */
        private val visited = new scala.collection.jcl.IdentityHashMap[ParamAttributeKey,Unit]

        /**
         * Return the value of this attribute for node t, raising an error if
         * it depends on itself.
         */
        def apply (arg : TArg) : T ==> U =
            new (T ==> U) {

                def apply (t : T) : U = {
                    val key = new ParamAttributeKey (arg, t)
                    if (visited contains key) {
                        throw new IllegalStateException ("Cycle detected in attribute evaluation")
                    } else {
                        visited (key) = ()
                        val u = f (arg) (t)
                        visited -= key
                        u
                    }
                }

                def isDefinedAt (t : T) =
                    f (arg) isDefinedAt t

            }
    }

    /**
     * Define an attribute of T nodes of type U by the function f, which
     * should not depend on the value of this attribute.  The computed
     * attribute value is cached so it will be computed at most once.
     */
    def attr[T <: AnyRef,U] (f : T ==> U) : T ==> U =
        new UncachedAttribute (f)

    /**
     * Define an attribute of T nodes of type U by the function f,
     * which takes an argument of type TArg.  The computed attribute value
     * for a given TArg is cached so it will be computed at most once.
     */
    def paramAttr[TArg,T <: AnyRef,U] (f : TArg => T ==> U) : TArg => T ==> U =
        new UncachedParamAttribute (f)

    /**
     * Define an attribute of T nodes of type U by the function f,
     * which takes the current node and its parent as its arguments.
     * T must be Attributable so that parents can be accessed.
     */
    def childAttr[T <: Attributable,U] (f : T => Attributable ==> U) : T ==> U =
        attr { case t => f (t) (t.parent) }

}
