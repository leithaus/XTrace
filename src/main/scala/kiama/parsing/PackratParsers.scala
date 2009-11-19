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

/**
 * Some parts of this file are based on code in the Scala Library, which is
 * distributed under the following copyright.
 *
 * Copyright (c) 2002-2008 EPFL, Lausanne, unless otherwise specified.
 * All rights reserved.
 *
 * (See the file COPYING.SCALA for details of the Scala license.)
 */

package kiama.parsing

/**
 * Parser combinator library modelled on the Scala parser combinator
 * library, and through it on various versions of parser combinator
 * libraries for Haskell.
 */
abstract trait Parsers {

    import scala.util.parsing.input._

    /**
     * The abstract type of input element that these parsers process.
     */
    type Elem

    /**
     * The input for these parsers comes from a reader of the element
     * type.  We use the same readers as the standard parser combinator
     * library.
     */
    type Input = Reader[Elem]

    /**
     * Representations of the results of parsing.
     */
    sealed abstract class ParseResult[+T] {

        /**
         * The input that remains to be consumed.
         */
        val in : Input

        /**
         * If this result reflects a successful parse, apply f to the value
         * produced and return the result of that application.
         */
        def map[U] (f : T => U) : ParseResult[U]

        /**
         * If this result reflects a successful parse, apply the partial function
         * f to the value produced and, if it's defined, return the result of that
         * application.  If f is not defined, produce a failure result with the
         * message produced by applying error to the result value.
         */
        def mapPartial[U] (f : PartialFunction[T,U], error : T => String) : ParseResult[U]

        /**
         * If this result reflects a successful parse, feed the resulting
         * value and the remainder of the input to f to obtain a final result.
         */
        def flatMapWithNext[U] (f : T => Input => ParseResult[U]) : ParseResult[U]

        /**
         * If this result reflects a successful parse, return it, otherwise
         * return a.
         */
        def append[U >: T] (a : => ParseResult[U]) : ParseResult[U]

        /**
         * Set the position of this result to that of in, if it is capable of
         * holding position information and doesn't already have some.
         */
        def position (in : Input) : ParseResult[T]

    }

    /**
     * A successful parse result.
     *
     * @param result a value representing the result of the parse
     * @param in the remainder of the input
     */
    case class Success[T] (result : T, in : Input) extends ParseResult[T] {
        def map[U] (f : T => U) =
            Success (f (result), in)
        def mapPartial[U] (f : PartialFunction[T,U], error : T => String) : ParseResult[U] =
            if (f.isDefinedAt (result))
                Success (f (result), in)
            else
                Failure (error (result), in)
        def flatMapWithNext[U] (f : T => Input => ParseResult[U]) : ParseResult[U] =
            f (result) (in)
        def append[U >: T] (a : => ParseResult[U]) : ParseResult[U] =
            this
        def position (in : Input) : ParseResult[T] = {
            result match {
                case p : Positional => if (p.pos == NoPosition) p.setPos (in.pos)
                case _              =>
            }
            this
        }
        override def toString = "[" + in.pos + "] parsed: " + result
    }

    /**
     * A parse result representing failure of a parse.
     *
     * @param msg a message describing the failure
     * @param in the remainder of the input
     */
    case class Failure (msg : String, in : Input) extends ParseResult[Nothing] {
        def map[U] (f : Nothing => U) =
            this
        def mapPartial[U] (f : PartialFunction[Nothing,U], error : Nothing => String) : ParseResult[U] =
            this
        def flatMapWithNext[U] (f : Nothing => Input => ParseResult[U]) : ParseResult[U] =
            this
        def append[U >: Nothing] (a : => ParseResult[U]) : ParseResult[U] =
            a
        def position (in : Input) : ParseResult[Nothing] =
            this
        override def toString = "[" + in.pos + "] failure: " + msg + "\n\n" + in.pos.longString
    }

    /**
     * A tuple for compound parser result values.  Designed to match the ~
     * combinator used for sequencing.
     */
    case class ~[+U,+V] (l : U, r : V)

    /**
     * A parser from inputs to parse results.
     */
    abstract class Parser[+T] extends (Input => ParseResult[T]) {

        /**
         * Alias this parser as p to make it easier to refer to in the
         * combinator definitions.
         */
        p =>

        /**
         * Run this parser.
         *
         * @param in the input on which the parser should run
         * @return the result of the parse
         */
        def apply (in : Input) : ParseResult[T]

        /**
         * Run this parser and, if the parse was successful, apply f to the
         * result.
         */
        def map[U] (f : T => U) : Parser[U] =
            Parser { in =>
                p (in) map (f)
            }

        /**
         * Run this parser and, if the parse was successful, feed the resulting
         * value to f to continue parsing.
         */
        def flatMap[U] (f : T => Parser[U]) : Parser[U] =
            Parser { in =>
                p (in) flatMapWithNext (f)
            }

        /**
         * Run this parser and, if the parse was successful, return its result.
         * Otherwise try parsing with q.
         */
        def append[U >: T] (q : => Parser[U]) : Parser[U] =
            Parser { in =>
                p (in) append q (in)
            }

        /**
         * Construct a parser that applies this parser and then q, returning
         * a tuple of the results if the parses succeed.
         */
        def ~[U] (q : => Parser[U]) : Parser[~[T, U]] =
            for (v <- p; w <- q) yield new ~ (v,w)

        /**
         * Construct a parser that applies this parser and then q, returning
         * the result of q if the parses succeed.
         */
        def ~>[U] (q : => Parser[U]) : Parser[U] =
            for (v <- p; w <- q) yield w

        /**
         * Construct a parser that applies this parser and then q, returning
         * the result of this parser if the parses succeed.
         */
        def <~[U] (q : => Parser[U]) : Parser[T] =
            for (v <- p; w <- q) yield v

        /**
         * Construct a parser that runs this parser and, if successful, passes
         * the result to f and runs the resulting parser, returning the result
         * of the second parser.
         */
        def >>[U] (f : T => Parser[U]) : Parser[U] =
            flatMap (f)

        /**
         * Construct a parser that parses zero or more occurrences of
         * what this parser parses.  Collect the result values in a
         * list.  Defined in terms of +.
         */
        def * : Parser[List[T]] =
            rep (p)

        /**
         * Construct a parser that parses zero or more occurrences of
         * what this parser parses, where each pair of occurrences is
         * separated by something that sep parses.  At the moment, there
         * is no way to get at the results of sep.
         */
        def *[U] (sep : => Parser[U]) : Parser[List[T]] =
             repsep (p, sep)

        /**
         * Construct a parser that parses one or more occurrences of
         * what this parser parses.  Collect the result values in a
         * list.  This parser is right recursive to avoid infinite
         * recursion.
         */
        def + : Parser[List[T]] =
            rep1 (p)

        /**
         * Construct a parser that parses one or more occurrences of
         * what this parser parses, where each pair of occurrences is
         * separated by something that sep parses.  At the moment, there
         * is no way to get at the results of sep.
         */
        def +[U] (sep : => Parser[U]) : Parser[List[T]] =
            rep1sep (p, sep)

        /**
         * Construct a parser that parsers either what this parser parses
         * or nothing.
         */
        def ? : Parser[Option[T]] =
            opt (p)

        /**
         * Construct a parser that tries to parse using this parser,
         * and if successful, returns the result of that parse.  If
         * the parse fails, try parsing with q.
         */
        def |[U >: T] (q : => Parser[U]) : Parser[U] =
            append (q)

        /**
         * Construct a parser that parses what this parser parses and,
         * if successful, applies f to the result. The starting position
         * is attached to the result if it holds position information
         * and doesn't already have some.
         */
        def ^^[U] (f : T => U) : Parser[U] =
            Parser { in => p (in) map (f) position in }

        /**
         * Construct a parser that parses what this parser parses and,
         * if it's successful, returns u.
         */
        def ^^^[U] (u : => U) : Parser[U] =
            ^^ (_ => u)

        /**
         * Construct a parser that runs this parser, and if the parse was
         * successful, applies the partial function f to the result.  If
         * f applies, return its result a successful parse result, otherwise
         * fail with a generic message.
         */
        def ^?[U] (f : PartialFunction[T,U]) : Parser[U] =
            ^? (f, r => "function not defined at " + r)

        /**
         * Construct a parser that runs this parser, and if the parse was
         * successful, applies the partial function f to the result.  If
         * f applies, return its result a successful parse result, otherwise
         * fail with the message produced by error from the result of this
         * parser.
         */
        def ^?[U] (f : PartialFunction[T,U], error : T => String) : Parser[U] =
            Parser { in =>
                p (in).mapPartial (f, error)
            }

        /**
         * Construct a parser that returns the result of parsing with this
         * parser, except that it unconditionally backtracks to the input position
         * when this parser was invoked.  I.e., the resulting parser is only useful
         * for its success or failure result, not its effect on the input.
         */
        def unary_+ : Parser[T] =
            and (p)

        /**
         * Construct a parser that succeeds if this parser fails and fails if
         * this parser succeeds.  In the case of success (i.e., this parser has
         * failed), the constructed parser returns ().
         */
        def unary_! : Parser[Unit] =
            not (p)

    }

    /**
     * Construct a parser that produces whatever result f produces when
     * applied to the input.
     */
    def Parser[T] (f : Input => ParseResult[T]) : Parser[T] =
        new Parser[T] { def apply (in : Input) = f (in) }

    /**
     * Construct a parser that always succeeds with the given result
     * without consuming any input.
     */
    def success[T] (result : T) : Parser[T] =
        Parser { in => Success (result, in) }

    /**
     * Construct a parser that always fails with the given message without
     * consuming any input.
     */
    def failure (message : String) : Parser[Nothing] =
        Parser { in => Failure (message, in) }

    /**
     * Parse any element.
     */
    val any : Parser[Elem] =
        (e : Elem) => true

    /**
     * (Implicitly) construct a parser that succeeds with e if the next input
     * element is e, and otherwise fails.
     */
    implicit def accept (e : Elem) : Parser[Elem] =
        Parser { in =>
            if (e == in.first)
                Success (in.first, in.rest)
            else
                Failure (e.toString, in)
        }

    /**
     * (Implicitly) construct a parser that succeeds if the given predicate
     * answers true when applied to the next input element, and otherwise
     * fails.
     */
    implicit def acceptIf (pred : Elem => Boolean) : Parser[Elem] =
        Parser { in =>
            if (pred (in.first))
                Success (in.first, in.rest)
            else
                Failure ("acceptIf", in)
        }

    /**
     * Construct a parser that parsers either what p parses or nothing.
     */
    def opt[T] (p : => Parser[T]) : Parser[Option[T]] =
        p ^^ (t => Some (t)) | success (None)

    /**
     * Construct a parser that parses zero or more occurrences of
     * what p parses.  Collect the result values in a list.
     * Defined in terms of rep1.
     */
    def rep[T] (p : => Parser[T]) : Parser[List[T]] =
        rep1 (p) | success (Nil)

    /**
     * Construct a parser that parses one or more occurrences of
     * what p parses.  Collect the result values in a list.
     * This parser is right recursive to avoid infinite recursion.
     */
    def rep1[T] (p : => Parser[T]) : Parser[List[T]] = {
        def q : Parser[List[T]] =
            (p ~ q) ^^ { case t ~ ts => t :: ts } |
            p ^^ (t => List (t))
        q
    }

    /**
     * Construct a parser that parses exactly n repetitions of what
     * p parses.  Collect the result values in a list.
     */
    def repN[T] (n : Int, p : => Parser[T]) : Parser[List[T]] =
        if (n == 0)
            success (Nil)
        else
            p ~ repN (n-1, p) ^^ { case t ~ ts => t :: ts }

    /**
     * Construct a parser that parses zero or more occurrences of
     * what p parses, where each pair of occurrences is separated
     * by something  that sep parses.  At the moment, there is no
     * way to get at the results of sep.
     */
    def repsep[T,U] (p : => Parser[T], sep : => Parser[U]) : Parser[List[T]] =
        rep1sep (p, sep) | success (Nil)

    /**
     * Construct a parser that parses one or more occurrences of
     * what p parses, where each pair of occurrences is separated
     * by something that sep parses.  At the moment, there is no
     * way to get at the results of sep.
     */
    def rep1sep[T,U] (p : => Parser[T], sep : => Parser[U]) : Parser[List[T]] =
        (p ~ ((sep ~> rep1sep (p, sep)) | success (Nil))) ^^
            { case t ~ ts => t :: ts }

    /**
     * Construct a parser that parses with p and then makes sure that the
     * entire input has been consumed (i.e., the input was a phrase that
     * was recognised by p).
     */
    def phrase[T] (p : => Parser[T]) : Parser[T] =
        new Parser[T] {
            def apply (in : Input) =
                p (in) match {
                    case s @ Success (_, in1) if in1.atEnd =>
                        s
                    case Success (_, in1) =>
                        Failure ("end of input expected", in1)
                    case f @ Failure (_, _) =>
                        f
                }
        }

    /**
     * Construct a parser that returns the result of parsing with p, except
     * that it unconditionally backtracks to the input position when p was
     * invoked.  I.e., the resulting parser is only useful for its success
     * or failure result, not its effect on the input.
     */
    def and[T] (p : => Parser[T]) : Parser[T] =
        new Parser[T] {
            def apply (in : Input) =
                p (in) match {
                    case Success (t, _) => Success (t, in)
                    case Failure (m, _) => Failure (m, in)
                }
        }

    /**
     * Construct a parser that succeeds if p fails and fails if p succeeds.
     * In the case of success (i.e., p has failed), the constructed parser
     * returns ().  In neither case is the input affected.
     */
    def not[T] (p : => Parser[T]) : Parser[Unit] =
        new Parser[Unit] {
            def apply (in : Input) =
                p (in) match {
                    case Success (t, _) => Failure ("predicate failure", in)
                    case Failure (_, _) => Success ((), in)
                }
        }

}

/**
 * Parsers that use the packrat parsing approach to memoise parsing
 * results, including support for left recursive grammar rules.
 *
 * The algorithms used here are from "Packrat parsers can support left
 * recursion" by Warth, Douglass and Millstein, ACM SIGPLAN Symposium on
 * Partial Evaluation and Semantics-based Program Manipulation, 2008.
 */
trait PackratParsers extends Parsers {

    import scala.collection.mutable.HashMap
    import scala.collection.mutable.ListBuffer
    import scala.collection.mutable.Set
    import scala.util.parsing.input.Position

    /**
     * Information about an active instance of left recursion.
     */
    case class Head (rule : Rule, var involvedSet : Set[Rule], var evalSet : Set[Rule])

    /**
     * Map between left input positions and active left recursion instances.
     */
    var heads = new HashMap[Input,Head]

    /**
     * Parsing answers.
     */
    abstract class Answer[T]

    /**
     * An answer that is a parser result.
     */
    case class Result[T] (result : ParseResult[T]) extends Answer[T]

    /**
     * An answer that is a left recursion record.
     */
    case class LR[T] (var seed : ParseResult[T], rule : Rule, var head : Head, next : LR[_]) extends Answer[T]

    /**
     * Left recursion stack.
     */
    var LRStack : LR[_] = null

    /**
     * Common supertype for all rules (ie regardless of result type).
     */
    trait Rule

    /**
     * A typed rule is a memoising, left recursion-detecting encapsulation
     * of a parser that returns a value of a particular type.
     */
    class MemoParser[T] (body : => Parser[T]) extends Parser[T] with Rule {

        /**
         * Alias this parser as p to make it easier to refer to in the
         * combinator definitions.
         */
        p =>

        /**
         * Memo table entries.
         */
        case class MemoEntry (var ans : Answer[T], var in : Input)

        /**
         * The section of the memo table relating to this rule.
         */
        val memo = new HashMap[Input,MemoEntry]

        /**
         * Apply this rule, memoising the result.
         */
        def apply (in : Input) : ParseResult[T] = {
            recall (in) match {
                case None =>
                    val lr = LR[T] (Failure ("left recursion", in), this, null, LRStack)
                    LRStack = lr
                    val m = MemoEntry (lr, in)
                    memo += (in -> m)
                    val ans = body (in)
                    LRStack = LRStack.next
                    m.in = ans.in
                    if (lr.head == null) {
                        m.ans = Result (ans)
                        ans
                    } else {
                        lr.seed = ans
                        lranswer (in, m)
                    }
                case Some (MemoEntry (Result (r), _)) =>
                    r
                case Some (MemoEntry (lr @ LR (_, _, _, _), _)) =>
                    setuplr (lr)
                    lr.seed
            }

        }

        /**
         * Initialise the left recursion data for a new application of this
         * rule.
         */
        def setuplr (l : LR[T]) {
            if (l.head == null)
                l.head = Head (p, Set (), Set ())
            var s = LRStack
            while (s.head != l.head) {
                s.head = l.head
                l.head.involvedSet = l.head.involvedSet + s.rule
                s = s.next
            }
        }

        /**
         * Process a given left recursion instance.
         */
        def lranswer (in : Input, m : MemoEntry) : ParseResult[T] = {
            m.ans match {
                case lr @ LR (_, _, _, _) =>
                    val h = lr.head
                    if (h.rule == p) {
                        m.ans = Result (lr.seed)
                        m.ans match {
                            case Result (f @ Failure (_, _)) =>
                                f
                            case _ =>
                                 growlr (in, m, h)
                        }
                    } else
                        lr.seed
                case _ =>
                    error ("lranswer: unexpected non-LR answer")
            }
        }

        /**
         * Look up the memoised result for this rule, taking into account that
         * it might be participating in an active left recursion.
         */
        def recall (in : Input) : Option[MemoEntry] = {
            val om = memo.get (in)
            heads.get (in) match {
                case None     => om
                case Some (h) =>
                    if ((om == None) && !((h.involvedSet + h.rule) contains p))
                        return Some (MemoEntry (Result (Failure ("left recursion skip", in)), in))
                    if (h.evalSet contains this) {
                        h.evalSet = h.evalSet - p
                        val ans = body (in)
                        memo += (in -> MemoEntry (Result (ans), ans.in))
                        memo.get (in)
                    } else
                        om
            }
        }

        /**
         * Grow the current parse result according to a left recursion.
         */
        def growlr (in : Input, m : MemoEntry, h : Head) : ParseResult[T] = {

            def nolater (p1 : Position, p2 : Position) : Boolean =
                (p1 == p2) || (p1 < p2)

            heads += (in -> h)
            while (true) {
                h.evalSet = h.involvedSet.clone
                val ans = body (in)
                if (ans.isInstanceOf[Failure] || nolater (ans.in.pos, m.in.pos)) {
                    heads -= in
                    m.ans match {
                        case Result (r) =>
                            return r
                        case _ =>
                            error ("growlr: unexpected non-result answer")
                    }
                }
                m.ans = Result (ans)
                m.in = ans.in
            }
            error ("growlr: went where I shouldn't go")
        }

        /**
         * Construct a parser that parses what this parser parses and,
         * if successful, applies f to the result. The starting position
         * is attached to the result if it holds position information
         * and doesn't already have some.
         */
        override def ^^[U] (f : T => U) : MemoParser[U] =
            super.^^ (f)

    }

    /**
     * Construct a parser that parses one or more occurrences of
     * what p parses.  Collect the result values in a list.  Because
     * these parsers are able to deal with left recursion and the
     * iteration used to handle left recursion is more efficient than
     * a general stack-based right recursion, left recursion is used here.
     */
    def rep1[T] (p : => MemoParser[T]) : MemoParser[List[T]] = {
        val l = new ListBuffer[T]
        def q : MemoParser[ListBuffer[T]] =
            (q ~ p) ^^ { case ts ~ t => ts += t; l } |
            p ^^ (t => { l += t; l })
        q ^^ (lb => lb.toList)
    }

    /**
     * (Implicit) conversion of non-memoising parser into a memoising one.
     */
    implicit def memo[T] (parser : => Parser[T]) : MemoParser[T] =
        new MemoParser[T] (parser)

}

/**
 * Parsers that operate on character input.
 */
trait CharParsers extends Parsers {

    import scala.util.matching.Regex
    import scala.util.parsing.input.{CharSequenceReader,PagedSeqReader}
    import scala.collection.immutable.PagedSeq

    /**
     * CharParsers parse character elements.
     */
    type Elem = Char

    /**
     * Parse a whitespace character.
     */
    lazy val whitespace : Parser[Char] =
        (ch : Char) => ch.isWhitespace

    /**
     * The layout to be allowed between tokens.  Defaults to zero or more
     * whitespace characters.  Override this to change the processing of
     * whitespace.
     */
    lazy val layout : Parser[List[Char]] =
        whitespace*

    /**
     * Parse whatever p parses followed by layout.
     */
    def token[T] (p : Parser[T]) : Parser[T] =
        p <~ layout

    /**
     * Construct a parser that parses with p and then makes sure that the
     * entire input has been consumed (i.e., the input was a phrase that
     * was recognised by p). Layout is allowed at the beginning.
     */
    override def phrase[T] (p : => Parser[T]) : Parser[T] =
        super.phrase (layout ~> p)

    /**
     * (Implicitly) construct a parser that succeeds if the next part
     * of the input is the given string, and otherwise fails.  Layout is
     * skipped after the string.
     */
    implicit def literal (s : String) : Parser[String] =
        token (Parser { in =>
            val source = in.source
            val offset = in.offset
            var i = 0
            var j = offset
            while (i < s.length && j < source.length && s.charAt (i) == source.charAt (j)) {
                i += 1
                j += 1
            }
            if (i == s.length)
                Success (source.subSequence (offset, j).toString, in.drop (j - offset))
            else
                Failure ("'" + s + "' expected", in)
        })

    /**
     * (Implicitly) construct a parser that succeeds if the next part of
     * the input matches the given regular expression, and otherwise fails.
     */
    implicit def regex (r: Regex) : Parser[String] =
        Parser { in =>
            val source = in.source
            val offset = in.offset
            (r findPrefixMatchOf (source.subSequence (offset, source.length))) match {
                case Some (matched) =>
                    Success (source.subSequence (offset, offset + matched.end).toString,
                             in.drop (matched.end))
                case None =>
                    Failure ("string matching regex `" + r + "' expected but `" + in.first + "' found", in)
            }
        }

    /**
     * Parse a digit character.
     */
    lazy val digit : Parser[Char] =
        (ch : Char) => ch.isDigit

    /**
     * Parse a letter character.
     */
    lazy val letter : Parser[Char] =
        (ch : Char) => ch.isLetter

    /**
     * Parse a letter or digit character.
     */
    lazy val letterOrDigit : Parser[Char] =
        (ch : Char) => ch.isLetterOrDigit

    /**
     * Run a parser on a given input and return its result.
     */
    def parse[T] (p : => Parser[T], in: Input) : ParseResult[T] =
        p (in)

    /**
     * Run a parser on a given character sequence and return its result.
     */
    def parse[T] (p : => Parser[T], in: java.lang.CharSequence) : ParseResult[T] =
        p (new CharSequenceReader (in))

    /**
     * Run a parser on a given character sequence and return its result.
     */
    def parse[T] (p : => Parser[T], in: java.io.Reader) : ParseResult[T] =
        p (new PagedSeqReader (PagedSeq.fromReader (in)))

    /**
     * Run a parser on a given input and return its result, requiring
     * that all of the input be consumed.
     */
    def parseAll[T] (p : => Parser[T], in: Input) : ParseResult[T] =
        parse (phrase (p), in)

    /**
     * Run a parser on a given reader and return its result, requiring
     * that all of the input be consumed.
     */
    def parseAll[T] (p : => Parser[T], in: java.io.Reader) : ParseResult[T] =
        parse (phrase (p), in)

    /**
     * Run a parser on a given character sequence and return its result, requiring
     * that all of the input be consumed.
     */
    def parseAll[T] (p : => Parser[T], in: java.lang.CharSequence): ParseResult[T] =
        parse (phrase (p), in)

}

/**
 * Packrat parsers that operate on character input.
 */
trait CharPackratParsers extends PackratParsers with CharParsers
