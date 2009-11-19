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

import scala.util.parsing.input.Positional

/**
 * Common functionality for all classes that are to be attributed.  This
 * trait must be extended by all classes for which the node properties
 * such as parent and the attribute shorthand notation <code>-></code>
 * are desired.
 */
trait Attributable extends Product with Positional {

    /**
     * A link to the parent Attributable node of this node or null if this
     * node has no parent.  Note that this link will skip intervening 
     * non-Attributable ancestors, such as <code>Option</code> or
     * <code>Seq</code> nodes.
     */
    var parent : Attributable = null

    /**
     * A short-hand for parent.asInstanceOf[T], which is useful in cases
     * a T-specific operation is applied to the parent, which otherwise
     * would be Attributable.
     */
    def parent[T] : T = parent.asInstanceOf[T]

    /**
     * Is this node the root of the hierarchy?
     */
    def isRoot : Boolean = parent == null

    /**
     * If this node is a member of a sequence, a link to the previous
     * node in the sequence.  Null if this is the first node of the
     * sequence, or if it is not a member of a sequence.
     */
    def prev : this.type = _prev.asInstanceOf[this.type]

    /**
     * Private field backing prev to make the types work correctly.
     */
    private var _prev : Attributable = null

    /**
     * If this node is a member of a sequence, a link to the next
     * node in the sequence.  Null if this is the first node of the
     * sequence, or if it is not a member of a sequence.
     */
    def next : this.type = _next.asInstanceOf[this.type]

    /**
     * Private field backing next to make the types work correctly.
     */
    var _next : Attributable = null

    /**
     * If this node is in a sequence, is it the first element?
     * Otherwise, true.
     */
    def isFirst : Boolean = prev == null

    /**
     * If this node is in a sequence, is it the last element?
     * Otherwise, true.
     */
    def isLast : Boolean = next == null

    /**
     * If this node is in a sequence, which child number is it
     * (counting from zero)?  Otherwise, zero.
     */
    var index : Int = 0

    /**
     * This node's attributable children in left-to-right order.  Children
     * that are not Attributable are ignored, except for sequences (<code>Seq[_]</code>)
     * and optional children (<code>Option[_]</code>).  In the case of sequences and
     * options, their contents are processed and any immediate Attributable
     * contents are included in the sequence.
     */
    def children : Iterator[Attributable] =
        _children.elements

    /**
     * Record of this node's attributable children.
     */
    private val _children = new scala.collection.mutable.ListBuffer[Attributable]

    /**
     * Reference an attribute or function that can be applied to this node.
     * <code>this->attribute</code> is equivalent to <code>attribute(this)</code>.
     */
    @inline
    final def ->[T] (attr : this.type => T) = attr (this)

    /**
     * House-keeping method to connect my children to me and their siblings.
     * If a node is a direct child of a <code>Seq</code> or <code>Some</code>,
     * then the parent link "bypasses" that parent to go to the <code>Attributable</code>
     * parent above.  It is assumed at that sequences and options are not directly nested.
     * As a side-effect, this method remembers the attributable children
     * so that they can be accessed easily via the children iterator.
     */
    private def setChildConnections = {

        for (i <- 0 until productArity) {
            productElement (i) match {
                case c : Attributable =>
                    c.parent = this
                    _children += c
                case o : Some[_] =>
                    o.get match {
                        case c : Attributable =>
                            c.parent = this
                            _children += c
                        case _ =>
                            // Ignore optional items that are non-Attributables
                    }
                case s : Seq[_] => {
                    var prev : Attributable = null
                    for (i <- 0 until s.length) {
                        s (i) match {
                            case c : Attributable =>
                                // Bypass Seq node in parent relation
                                c.parent = this
                                _children += c
                                // Set sequence element properties
                                c.index = i
                                c._prev = prev
                                if (prev != null) prev._next = c
                                prev = c
                            case _ =>
                                // Ignore elements that are non-Attributables
                        }
                    }
                }
                case _ =>
                    // Ignore children that are not Attributable, options or sequences
            }
        }

    }

    setChildConnections

}
