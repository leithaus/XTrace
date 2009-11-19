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

package kiama.rewriting

/**
 * Strategy-based term rewriting in the style of Stratego (http://strategoxt.org/).
 * The implementation here is partially based on the semantics given in "Program
 * Transformation with Scoped Dynamic Rewrite Rules", by Bravenboer, van Dam, Olmos
 * and Visser, Fundamenta Informaticae, 69, 2005. The library strategies are mostly
 * based on the Stratego library, but also on combinators found in the Scrap Your
 * Boilerplate and Uniplate libraries for Haskell.
 */
trait Rewriter {

    /**
     * The type of terms that can be rewritten.  Any type of object value is
     * acceptable but generic traversals will only work on Products (e.g.,
     * instances of case classes).  We use AnyRef here so that non-Product
     * values can be present in rewritten structures.
     */
    type Term = AnyRef

    /**
     * Term-rewriting strategies.
     */
    abstract class Strategy extends (Term => Option[Term]) {

        /**
         * Alias this strategy as p to make it easier to refer to in the
         * combinator definitions.
         */
        p =>

        /**
         * Apply this strategy to a term, producing either a transformed term
         * or None, representing a rewriting failure.
         */
        def apply (r : Term) : Option[Term]

        /**
         * Sequential composition.  Construct a strategy that first applies
         * this strategy. If it succeeds, then apply q to the new subject
         * term.  Otherwise fail.
         */
        def <* (q : => Strategy) : Strategy =
            new Strategy {
                def apply (t1 : Term) =
                    p (t1) match {
                        case Some (t2) => q (t2)
                        case None      => None
                    }
            }

        /**
         * Deterministic choice.  Construct a strategy that first applies
         * this strategy.  If it succeeds, succeed with the resulting term.
         * Otherwise, apply q to the original subject term.
         */
        def <+ (q : => Strategy) : Strategy =
            new Strategy {
                def apply (t1 : Term) =
                    p (t1) match {
                        case Some (t2) => Some (t2)
                        case None      => q (t1)
                    }
            }

        /**
         * Non-deterministic choice.  Normally, construct a strategy that
         * first applies either this strategy or the given strategy.  If it
         * succeeds, succeed with the resulting term. Otherwise, apply q.
         * Currently implemented as deterministic choice, but this behaviour
         * should not be relied upon.
         * When used as the argument to the <code>&lt;</code> conditional
         * choice combinator, <code>+</code> just serves to hold the two
         * strategies that are chosen between by the conditional choice.
         */
        def + (q : => Strategy) : PlusStrategy =
            new PlusStrategy (p, q)

        /**
         * Conditional choice: c < l + r.
         * Construct a strategy that first applies this
         * strategy (c). If it succeeds, apply l to the resulting term,
         * otherwise apply r to the original subject term.
         */
        def < (lr : => PlusStrategy) : Strategy =
            new Strategy {
                def apply (t1 : Term) =
                    p (t1) match {
                        case Some (t2) => lr.lhs (t2)
                        case None      => lr.rhs (t1)
                    }
            }

    }

    /**
     * Helper class to contain commonality of choice in non-deterministic
     * choice operator and then-else part of a conditional choice.  Only
     * returned by the non-deterministic choice operator.
     */
    class PlusStrategy (p : => Strategy, q : => Strategy) extends Strategy {
        val lhs = p
        val rhs = q
        def apply (t : Term) = (p <+ q) (t)
    }

    /**
     * Make a strategy from a function.  The function return value determines
     * whether the strategy succeeds or fails.
     */
    def strategyf (f : Term => Option[Term]) : Strategy =
        new Strategy {
            def apply (t : Term) = f (t)
        }

    /**
     * Make a strategy from a partial function.  If the function is
     * defined at the current term, then the function return value
     * when applied to the current term determines whether the strategy
     * succeeds or fails.  If the function is not defined at the current
     * term, the strategy fails.
     */
    def strategy (f : PartialFunction[Term,Option[Term]]) : Strategy =
        new Strategy {
            def apply (t : Term) = {
                if (f isDefinedAt t) {
                    f (t)
                } else {
                    None
                }
            }
        }

    /**
     * Define a rewrite rule using a function.  The rule always succeeds
     * with the return value of the function.
     */
    def rulef (f : Term => Term) : Strategy =
        strategyf (t => Some (f (t)))

    /**
     * Define a rewrite rule using a partial function.  If the function is
     * defined at the current term, then the strategy succeeds with the return
     * value of the function applied to the current term.  Otherwise the
     * strategy fails.
     */
    def rule (f : PartialFunction[Term,Term]) : Strategy =
        new Strategy {
            def apply (t : Term) = {
                if (f isDefinedAt t) {
                    Some (f (t))
                } else {
                    None
                }
            }
        }

    /**
     * (Implicitly) construct a strategy that always succeeds, changing
     * the subject term to a given term.
     */
    implicit def termToStrategy (t : Term) : Strategy =
         strategyf (_ => Some (t))

    /**
     * Define a term query.  Construct a strategy that always succeeds with no
     * effect on the subject term but applies a given (possibly partial)
     * function f to the subject term.  In other words, the strategy runs f
     * for its side-effects.
     */
    def queryf[T] (f : Term => T) : Strategy =
        new Strategy {
            def apply (t : Term) = {
                f (t)
                Some (t)
            }
        }

    /**
     * Define a term query.  Construct a strategy that always succeeds with no
     * effect on the subject term but applies a given partial function f to the
     * subject term.  In other words, the strategy runs f for its side-effects.
     */
    def query[T] (f : PartialFunction[Term,T]) : Strategy =
        new Strategy {
            def apply (t : Term) = {
                if (f isDefinedAt t) {
                    f (t)
                }
                Some (t)
            }
        }

    /**
     * A strategy that always fails.  Stratego's fail is avoided here to
     * avoid a clash with JUnit's method of the same name.
     */
    val failure : Strategy =
        strategyf (_ => None)

    /**
     * A strategy that always succeeds with the subject term unchanged (i.e.,
     * this is the identity strategy).
     */
    val id : Strategy =
        strategyf (t => Some (t))

    /**
     * Construct a strategy that succeeds only if the subject term matches
     * a given term.
     */
    def term (t : Term) : Strategy =
        rule {
            case `t` => t
        }

    /**
     * Generic term deconstruction.
     */
    object Term {

        /**
         * Generic term deconstruction.  An extractor that decomposes Products
         * into the product itself and a sequence of its children.  Terms that
         * are not products are not decomposable (ie the list of children will
         * be empty).
         */
        def unapply (t : Any) : Option[(Any,Seq[Any])] = {
            t match {
                case p : Product => {
                    val cs = for (i <- 0 until p.productArity) yield p.productElement (i)
                    Some ((p, cs))
                }
                case _ =>
                    Some ((t, Nil))
            }
        }

    }

    /**
     * Perform a paramorphism over a value.  This is a fold in which the
     * recursive step may refer to the recursive component of the value
     * and the results of folding over the children.  When the function f
     * is called, the first parameter is the value and the second is a
     * sequence of the values that f has returned for the children.  This
     * will work on any value, but will only decompose Products.  This
     * operation is similar to that used in the Uniplate library.
     */
    def para[T] (f : (Any, Seq[T]) => T) : Any => T = {
        case Term (t, ts) => f (t, ts.map (para (f)))
    }

    /**
     * General product duplication function.  Returns a product that applies
     * the same constructor as the product t, but with the given children
     * instead of t's children.  Fails if a constructor cannot be found or
     * if one of the children is not of the appropriate type.
     */
    private def dup (t : Product, children : Array[AnyRef]) : Product = {
        val ctor = (t.getClass.getConstructors())(0)
        try {
            ctor.newInstance (children : _*).asInstanceOf[Product]
        } catch {
            case e : java.lang.ClassCastException =>
                error ("dup cast failed: " + t)
            case e : IllegalArgumentException =>
                error ("dup illegal arguments: " + ctor + " " + children.deepMkString (",") +
                       " (expects " + ctor.getParameterTypes.length + ")")
        }
    }

    /**
     * Make an arbitrary value into a term child, checking that it worked properly.
     * Object references will be returned unchanged; other values will be boxed.
     */
    private def makechild (child : Any) : AnyRef = {
        try {
            return child.asInstanceOf[AnyRef]
        } catch {
            case e : ClassCastException =>
                error ("makechild: can't cast child: " + child + " " + e)
        }
    }

    /**
     * Traversal to a single child.  Construct a strategy that applies s to
     * the ith child of the subject term (counting from one).  If s succeeds on
     * the ith child producing t, then succeed, forming a new term that is the
     * same as the original term except that the ith child is now t.  If s fails
     * on the ith child or the subject term does not have an ith child, then fail.
     * child (i, s) is equivalent to Stratego's i(s) operator.
     */
    def child (i : Int, s : Strategy) : Strategy =
        new Strategy {
            def apply (t : Term) : Option[Term] = {
                t match {
                    case p : Product =>
                        val numchildren = p.productArity
                        if ((i < 1) || (i > numchildren)) {
                            None
                        } else {
                            p.productElement (i-1) match {
                                case ct : Term =>
                                    val children = new Array[AnyRef](numchildren)
                                    for (j <- 0 until numchildren)
                                        children (j) = makechild (p.productElement (j))
                                    s (ct) match {
                                        case Some (ti) =>
                                            children (i-1) = ti
                                        case None      =>
                                            return None
                                    }
                                    val ret = dup (p, children)
                                    Some (ret)
                                case ci =>
                                    None
                            }
                        }
                    case a =>
                        None
                }

            }
        }


    /**
     * Traversal to all children.  Construct a strategy that applies s to all
     * term children of the subject term in left-to-right order.  If s succeeds
     * on all of the children, then succeed, forming a new term from the constructor
     * of the original term and the result of s for each child.  If s fails on any
     * child, fail.
     */
    def all (s : => Strategy) : Strategy =
        new Strategy {
            def apply (t : Term) : Option[Term] = {
                t match {
                    case p : Product =>
                        val numchildren = p.productArity
                        if (numchildren == 0) {
                            Some (t)
                        } else {
                            val children = new Array[AnyRef](numchildren)
                            for (i <- 0 until numchildren) {
                                p.productElement (i) match {
                                    case ct : Term =>
                                        s (ct) match {
                                            case Some (ti) =>
                                                children (i) = ti
                                            case None      =>
                                                return None
                                        }
                                    case ci =>
                                        // Child is not a term, don't try to transform it
                                        children (i) = makechild (ci)
                                }
                            }
                            val ret = dup (p, children)
                            Some (ret)
                        }
                    case a =>
                        Some (a)
                }
            }
        }

    /**
     * Traversal to one child.  Construct a strategy that applies s to the term
     * children of the subject term in left-to-right order.  Assume that c is the
     * first child on which s succeeds.  Then stop applying s to the children and
     * succeed, forming a new term from the constructor of the original term and
     * the original children, except that c is replaced by the result of applying
     * s to c.  In the event that the strategy fails on all children, then fail.
     */
    def one (s : => Strategy) : Strategy =
        new Strategy {
            def apply (t : Term) : Option[Term] = {
                t match {
                    case p : Product =>
                        val numchildren = p.productArity
                        for (i <- 0 until numchildren) {
                            p.productElement (i) match {
                                case ct : Term =>
                                    s (ct) match {
                                        case Some (nct) => {
                                            val children = new Array[AnyRef] (numchildren)
                                            for (j <- 0 until i)
                                                children (j) = makechild (p.productElement (j))
                                            children (i) = nct
                                            for (j <- i + 1 until numchildren)
                                                children (j) = makechild (p.productElement (j))
                                            val ret = dup (p, children)
                                            return Some (ret)
                                        }
                                        case None =>
                                            // Transformation failed, this child stays unchanged
                                    }
                                case _ =>
                                    // Child is not a term, don't try to transform it
                            }
                        }
                        None
                    case _ =>
                        None
                }
            }
        }

    /**
     * Traversal to as many children as possible, but at least one.  Construct a
     * strategy that applies s to the term children of the subject term in
     * left-to-right order.  If s succeeds on any of the children, then succeed,
     * forming a new term from the constructor of the original term and the result
     * of s for each succeeding child, with other children unchanged.  In the event
     * that the strategy fails on all children, then fail.
     */
    def some (s : => Strategy) : Strategy =
        new Strategy {
            def apply (t : Term) : Option[Term] = {
                t match {
                    case p : Product =>
                        val numchildren = p.productArity
                        if (numchildren == 0) {
                            None
                        } else {
                            val children = new Array[AnyRef](numchildren)
                            var success = false
                            for (i <- 0 until numchildren) {
                                p.productElement (i) match {
                                    case ct : Term =>
                                        s (ct) match {
                                            case Some (ti) =>
                                                children (i) = ti
                                                success = true
                                            case None      =>
                                                // Child failed, don't transform it
                                                children (i) = ct
                                        }
                                    case ci =>
                                        // Child is not a term, don't try to transform it
                                        children (i) = makechild (ci)
                                }
                            }
                            if (success) {
                                val ret = dup (p, children)
                                Some (ret)
                            } else {
                                None
                            }
                        }
                    case _ =>
                        None
                }
            }
        }

    /**
     * Rewrite a term.  Apply the strategy s to a term returning the result term
     * if s succeeds, otherwise return the original term.
     */
    def rewrite[T <: Term] (s : => Strategy) (t : T) : T = {
        s (t) match {
            case Some (t1) =>
                t1.asInstanceOf[T]
            case None =>
                t
        }
    }

    /**
     * Collect query results in a set.  Run the function f as a top-down
     * query on the subject term.  Accumulate the values produced by the
     * function in a set and return the final value of the set.
     */
    def collects[T] (f : PartialFunction[Term,T]) : Term => Set[T] =
        (t : Term) => {
            var collection = Set[T]()
            def collect = (v : T) => collection += v
            (everywheretd (query (f andThen collect))) (t)
            collection
        }

    /**
     * Collect query results in a list.  Run the function f as a top-down
     * query on the subject term.  Accumulate the values produced by the
     * function in a list and return the final value of the list.
     */
    def collectl[T] (f : PartialFunction[Term,T]) : Term => List[T] =
        (t : Term) => {
            var collection = List[T]()
            def collect = (v : T) => collection = collection ::: List (v)
            (everywheretd (query (f andThen collect))) (t)
            collection
        }

    /**
     * Count function results.  Run the function f as a top-down query on
     * the subject term.  Sum the integer values returned by f from all
     * applications.
     */
    def count (f : PartialFunction[Term,Int]) : Term => Int =
        (t : Term) => {
            var total = 0
            def count = (v : Int) => total += v
            (everywheretd (query (f andThen count))) (t)
            total
        }

    /**
     * Construct a strategy that applies s, yielding the result of s if it
     * succeeds, otherwise leave the original subject term unchanged.  In
     * Stratego library this strategy is called "try".
     */
    def attempt (s : => Strategy) : Strategy =
        s <+ id

    /**
     * Construct a strategy that applies s repeatedly until it fails.
     */
    def repeat (s : => Strategy) : Strategy =
        attempt (s <* repeat (s))

    /**
     * Construct a strategy that repeatedly applies s until it fails and
     * then terminates with application of c.
     */
    def repeat (s : => Strategy, c : => Strategy) : Strategy =
        (s <* repeat (s, c)) <+ c

    /**
     * Construct a strategy that applies s repeatedly exactly n times. If
     * s fails at some point during the n applications, the entire strategy
     * fails. The result of the strategy is that of the nth application of s.
     */
    def repeat (s : => Strategy, n : Int) : Strategy =
        if (n == 0) id else s <* repeat (s, n - 1)

    /**
     * Construct a strategy that repeatedly applies s (at least once) and
     * terminates with application of c.
     */
    def repeat1 (s : => Strategy, c : => Strategy) : Strategy =
        s <* (repeat1 (s, c) <+ c)

    /**
     * Construct a strategy that repeatedly applies s (at least once).
     */
    def repeat1 (s : => Strategy) : Strategy =
        repeat1 (s, id)

    /**
     * Construct a strategy that repeatedly applies s until c succeeds.
     */
    def repeatuntil (s : => Strategy, c : => Strategy) : Strategy =
        s <* (c <+ repeatuntil (s, c))

    /**
     * Construct a strategy that while c succeeds applies s.  This operator
     * is called "while" in the Stratego library.
     */
    def loop (c : => Strategy, s : => Strategy) : Strategy =
        attempt (c <* s <* loop (c, s))

    /**
     * Construct a strategy that while c does not succeed applies s.  This
     * operator is called "while-not" in the Stratego library.
     */
    def loopnot (c : => Strategy, s : => Strategy) : Strategy =
        c <+ (s <* loopnot (c, s))

    /**
     * Construct a strategy that applies s at least once and then repeats s
     * while c succeeds.  This operator is called "do-while" in the Stratego
     * library.
     */
    def doloop (s : => Strategy, c : => Strategy) : Strategy =
       s <* loop (c, s)

    /**
     * Construct a strategy that repeats application of s while c fails, after
     * initialization with i.  This operator is called "for" in the Stratego
     * library.
     */
    def loopiter (i : => Strategy, c : => Strategy, s : => Strategy) : Strategy =
        i <* loopnot (c, s)

    /**
     * Construct a strategy that applies s (i) for each integer i from low to
     * up (inclusive).  This operator is called "for" in the Stratego library.
     */
    def loopiter (s : Int => Strategy, low : Int, up : Int) : Strategy =
        if (low <= up)
            s (low) <* loopiter (s, low + 1, up)
        else
            id

    /**
     * Construct a strategy that applies s, then fails if s succeeded or, if s
     * failed, succeeds with the subject term unchanged,  I.e., it tests if
     * s applies, but has no effect on the subject term.
     */
    def not (s : => Strategy) : Strategy =
        s < failure + id

    /**
     * Construct a strategy that tests whether strategy s succeeds,
     * restoring the original term on success.  This is similar
     * to Stratego's "where", except that in this version any effects on
     * bindings are not visible outside s.
     */
    def where (s : => Strategy) : Strategy =
        strategyf (t => (s <* t) (t))

    /**
     * Construct a strategy that tests whether strategy s succeeds,
     * restoring the original term on success.  A synonym for where.
     */
    def test (s : => Strategy) : Strategy =
        where (s)

    /**
     * Construct a strategy that applies s in breadth first order.
     */
    def breadthfirst (s : => Strategy) : Strategy =
        all (s) <* all (breadthfirst (s))

    /**
     * Construct a strategy that applies s in a top-down, prefix fashion
     * to the subject term.
     */
    def topdown (s : => Strategy) : Strategy =
        s <* all (topdown (s))

    /**
     * Construct a strategy that applies s in a top-down, prefix fashion
     * to the subject term but stops when stop succeeds.
     */
    def topdownS (s : => Strategy, stop : Strategy => Strategy) : Strategy =
        s <* (stop (topdownS (s, stop)) <+ all (topdownS (s, stop)))

    /**
     * Construct a strategy that applies s in a bottom-up, postfix fashion
     * to the subject term.
     */
    def bottomup (s : => Strategy) : Strategy =
        all (bottomup (s)) <* s

    /**
     * Construct a strategy that applies s in a bottom-up, postfix fashion
     * to the subject term but stops when stop succeeds.
     */
    def bottomupS (s : => Strategy, stop : Strategy => Strategy) : Strategy =
        (stop (bottomupS (s, stop)) <+ all (bottomupS (s, stop))) <* s

    /**
     * Construct a strategy that applies s in a combined top-down and
     * bottom-up fashion (i.e., both prefix and postfix) to the subject
     * term.
     */
    def downup (s : => Strategy) : Strategy =
        s <* all (downup (s)) <* s

    /**
     * Construct a strategy that applies s1 in a top-down, prefix fashion
     * and s2 in a bottom-up, postfix fashion to the subject term.
     */
    def downup (s1 : => Strategy, s2 : => Strategy) : Strategy =
        s1 <* all (downup (s1, s2)) <* s2

    /**
     * Construct a strategy that applies s in a combined top-down and
     * bottom-up fashion (i.e., both prefix and postfix) to the subject
     * term but stops when stop succeeds.
     */
    def downupS (s : => Strategy, stop : Strategy => Strategy) : Strategy =
        s <* (stop (downupS (s, stop)) <+ all (downupS (s, stop))) <* s

    /**
     * Construct a strategy that applies s1 in a top-down, prefix fashion
     * and s2 in a bottom-up, postfix fashion to the subject term but stops
     * when stop succeeds.
     */
    def downupS (s1 : => Strategy, s2 : => Strategy, stop : Strategy => Strategy) : Strategy =
        s1 <* (stop (downupS (s1, s2, stop)) <+ all (downupS (s1, s2, stop))) <* s2

    /**
     * A unit for topdownS, bottomupS and downupS.  For example, topdown (s)
     * is equivalent to topdownS (s, dontstop).
     */
    def dontstop (s : => Strategy) : Strategy =
        failure

    /**
     * Construct a strategy that applies s in a top-down fashion to one
     * subterm at each level, stopping as soon as it succeeds once (at
     * any level).
     */
    def oncetd (s : => Strategy) : Strategy =
        s <+ one (oncetd (s))

    /**
     * Construct a strategy that applies s in a bottom-up fashion to one
     * subterm at each level, stopping as soon as it succeeds once (at
     * any level).
     */
    def oncebu (s : => Strategy) : Strategy =
        one (oncebu (s)) <+ s

    /**
     * Construct a strategy that applies s in a top-down fashion to some
     * subterms at each level, stopping as soon as it succeeds once (at
     * any level).
     */
    def sometd (s : => Strategy) : Strategy =
        s <+ some (sometd (s))

    /**
     * Construct a strategy that applies s in a bottom-up fashion to some
     * subterms at each level, stopping as soon as it succeeds once (at
     * any level).
     */
    def somebu (s : => Strategy) : Strategy =
        some (somebu (s)) <+ s

    /**
     * Construct a strategy that applies s repeatedly in a top-down fashion
     * stopping each time as soon as it succeeds once (at any level). The
     * outermost fails when s fails to apply to any (sub-)term.
     */
    def outermost (s : => Strategy) : Strategy =
        repeat (oncetd (s))

    /**
     * Construct a strategy that applies s repeatedly to the innermost
     * (i.e., lowest and left-most) (sub-)term to which it applies.
     * Stop with the current term if s doesn't apply anywhere.
     */
    def innermost (s : => Strategy) : Strategy =
        bottomup (attempt (s <* innermost (s)))

    /**
     * An alternative version of innermost.
     */
    def innermost2 (s : => Strategy) : Strategy =
        repeat (oncebu (s))

    /**
     * Construct a strategy that applies s repeatedly to subterms
     * until it fails on all of them.
     */
    def reduce (s : => Strategy) : Strategy = {
        def x : Strategy = some (x) + s
        repeat (x)
    }

    /**
     * Construct a strategy that applies s in a top-down fashion, stopping
     * at a frontier where s succeeds.
     */
    def alltd (s : => Strategy) : Strategy =
        s <+ all (alltd (s))

   /**
     * Construct a strategy that applies s1 in a top-down, prefix fashion
     * stopping at a frontier where s succeeds.  s2 is applied in a bottom-up,
     * postfix fashion to the result.
     */
    def alldownup2 (s1 : => Strategy, s2 : => Strategy) : Strategy =
        (s1 <+ all (alldownup2 (s1, s2))) <* s2

    /**
     * Construct a strategy that applies s1 in a top-down, prefix fashion
     * stopping at a frontier where s succeeds.  s2 is applied in a bottom-up,
     * postfix fashion to the results of the recursive calls.
     */
    def alltdfold (s1 : => Strategy, s2 : => Strategy) : Strategy =
        s1 <+ (all (alltdfold (s1, s2)) <* s2)

   /**
     * Construct a strategy that applies s1 in a top-down, prefix fashion
     * stopping at a frontier where s succeeds on some children.  s2 is applied in a bottom-up,
     * postfix fashion to the subject term the result.
     */
   def somedownup (s : => Strategy) : Strategy =
       (s <* all (somedownup (s)) <* (attempt (s))) <+ (some (somedownup (s)) <+ attempt (s))

    /**
     * Construct a strategy that applies s as many times as possible, but
     * at least once, in bottom up order.
     */
    def manybu (s : Strategy) : Strategy =
        some (manybu (s)) <* (attempt (s) <+ s)

    /**
     * Construct a strategy that applies s as many times as possible, but
     * at least once, in top down order.
     */
    def manytd (s : Strategy) : Strategy =
        s <* all (attempt (manytd (s))) <+ some (manytd (s))

    /**
     * Construct a strategy that tests whether the two sub-terms of a
     * pair of terms are equal.
     */
    val eq : Strategy =
       rule {
           case t @ (x, y) if x == y => t
       }

    /**
     * Construct a strategy that tests whether the two sub-terms of a
     * pair of terms are equal. Synonym for eq.
     */
    val equal : Strategy =
        eq

    /**
     * Construct a strategy that succeeds when applied to a pair (x,y)
     * if x is a sub-term of y.
     */
    val issubterm : Strategy =
        strategy {
            case (x : Term, y : Term) => where (oncetd (term (x))) (y)
        }

    /**
     * Construct a strategy that succeeds when applied to a pair (x,y)
     * if x is a sub-term of y but is not equal to y.
     */
    val ispropersubterm : Strategy =
        not (eq) <* issubterm

    /**
     * Construct a strategy that succeeds when applied to a pair (x,y)
     * if x is a superterm of y.
     */
    val issuperterm : Strategy =
        strategy {
            case (x, y) => issubterm (y, x)
        }

    /**
     * Construct a strategy that succeeds when applied to a pair (x,y)
     * if x is a super-term of y but is not equal to y.
     */
    val ispropersuperterm : Strategy =
        not (eq) <* issuperterm

    /**
     * Construct a strategy that succeeds if the current term has no
     * direct subterms.
     */
    val isleaf : Strategy =
      all (failure)

    /**
     * Construct a strategy that applies to all of the leaves of the
     * current term, using isleaf as the leaf predicate.
     */
    def leaves (s : => Strategy, isleaf : => Strategy) : Strategy =
        (isleaf <* s) <+ all (leaves (s, isleaf))

    /**
     * Construct a strategy that applies to all of the leaves of the
     * current term, using isleaf as the leaf predicate, skipping
     * subterms for which skip succeeds.
     */
    def leaves (s : => Strategy, isleaf : => Strategy, skip : Strategy => Strategy) : Strategy =
        (isleaf <* s) <+ skip (leaves (s, isleaf)) <+ all (leaves (s, isleaf))

    /**
     * Construct a strategy that succeeds if the current term has at
     * least one direct subterm.
     */
    val isinnernode : Strategy =
        one (id)

    /**
     * Construct a strategy that applies s at every term in a bottom-up fashion
     * regardless of failure.  (Sub-)terms for which the strategy fails are left unchanged.
     */
    def everywherebu (s : => Strategy) : Strategy =
        bottomup (attempt (s))

    /**
     * Construct a strategy that applies s at every term in a top-down fashion regardless
     * of failure.  (Sub-)terms for which the strategy fails are left unchanged.
     */
    def everywheretd (s : => Strategy) : Strategy =
        topdown (attempt (s))

   /**
    * Apply restoring action 'rest' if s fails, and then fail.
    * Typically useful if s performs side effects that should be
    * restored/undone in case s fails.
    */
    def restore (s : => Strategy, rest : => Strategy) : Strategy =
        s <+ (rest <* failure)

    /**
     * Apply restoring action 'rest' after s terminates, and preserve
     * success/failure behaviour of s.
     * Typically useful if s performs side effects that should be
     * restored always, e.g., when maintaining scope information.
     */
    def restorealways (s : => Strategy, rest : => Strategy) : Strategy =
        s < rest + (rest <* failure)

    /**
     * Applies s followed by f whether s failed or not.
     * This operator is called "finally" in the Stratego library.
     */
    def lastly (s : => Strategy, f : => Strategy) : Strategy =
        s < where (f) + (where (f) <* failure)

    /**
     * ior (s1, s2) implements 'inclusive or', that is, the
     * inclusive choice of s1 and s2. It first tries s1, if
     * that fails it applies s2 (just like s1 <+ s2). However,
     * when s1 succeeds it also tries to apply s2.
     * The results of the transformations are returned.
     */
    def ior (s1 : => Strategy, s2 : => Strategy) : Strategy =
        (s1 <* attempt (s2)) <+ s2

    /**
     * or (s1, s2) is similar to ior (s1,s2), but the application
     * of the strategies is only tested.
     */
    def or (s1 : => Strategy, s2 : => Strategy) : Strategy =
        where (s1) < attempt (test (s2)) + test (s2)

    /**
     * and (s1, s2) applies s1 and s2 to the current
     * term and succeeds if both succeed. s2 will always
     * be applied, i.e., and is *not* a short-circuit
     * operator
     */
    def and (s1 : => Strategy, s2 : => Strategy) : Strategy =
        where (s1) < test (s2) + (test (s2) <* failure)

}
