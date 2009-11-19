/******************************************************************************* 
 * This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 *     Anastasia Izmaylova, 
 *     Miguel Garcia, http://www.sts.tu-harburg.de/people/mi.garcia 
 *******************************************************************************/ 

package sts.compilerPlugin

import scala.tools.nsc._
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.InfoTransform

import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.util.BatchSourceFile

import sts.ferry.AbstractSyntax._

/**
 * This compiler plugin is aimed to perform LINQ nested queries to Scala comprehensions
 * 	by firstly parsing a target LINQ query (thanks to LINQ Parser by Miguel Garcia) 
 * 	resulting in the corresponding scala syntax related string, and following parsing
 * 	this string (by means of scalac mechanisms) to the AST nodes that are to be inserted
 * 	in the AST of the original compilation unit 
 */
class ScalaASTTransformer(val global: Global) extends PluginComponent {

  import global._
  import global.definitions._
  import typer.{typed, atOwner}
  import posAssigner.atPos

  // it is expected to perform interruption after initial parse phase
  val runsAfter = "parser"

  val phaseName = "ScalaQLPhase"
  
  private var cur_phase: StdPhase = null
  private var cur_unit: CompilationUnit = null
  private var unit_inject: CompilationUnit = null 
  
  def newPhase(prev: Phase): Phase = new ScalaQLPhase(prev)
  
  class ScalaQLPhase(prev: Phase) extends StdPhase(prev) {
    def apply(unit: CompilationUnit) {
      cur_phase = new LINQToScala(prev)
      cur_phase.apply(unit)
      cur_phase = new ScalaToFerry(cur_phase)
      cur_phase.apply(cur_unit)
    }
  }
  
  class LINQToScala(prev: Phase) extends StdPhase(prev) {
    def apply(unit: CompilationUnit) {
      println("TREE BEFORE LINQ-TO-SCALA TRANSFORMATION: ")
      println(unit.body)
      cur_unit = unit
      ScalaToFerryParser.unit.body = ScalaToFerryParser.parse
      val transformer = newTransformer(unit)
      val newBody = transformer.transform(unit.body)
      unit.body = newBody
      println("TREE AFTER LINQ-TO-SCALA TRANSFORMATION: ")
      println(unit.body)
    }
  }
  
  class ScalaToFerry(prev: Phase) extends StdPhase(prev) {
    def apply(unit: CompilationUnit) {
      val transformer = newTransformer(unit)
      transformer.translate()
    }
  }
  
  
  def newTransformer(unit: CompilationUnit) = new LINQToScalaTransformer
  
  class LINQToScalaTransformer extends Transformer {
	import global._                 
    import definitions._                 
     
    override def transform(tree: Tree): Tree = {
    val newTree = super.transform(tree);      
    
    newTree match {
      
        
        case Template(parents,self, body) => 
          	// println("TEMPLATE")
          	copy.Template(newTree,parents,self,stmtsWalk(body))
        
        
        case Block(stats, expr) =>  
          	// println("BLOCK")
        	copy.Block(tree,stmtsWalk(stats),expr)
        
        
        case _ =>
        	newTree
      }
    
   }
    
   private def stmtsWalk(body:List[Tree]):List[Tree] = {
     
     // println("(1) template or block is matched ...")
         
     var body1:List[Tree] = List() 
        	
     for (tree <- body) {
       
       tree match {
         
         case e @ ValDef(Modifiers(_,_,List(Annotation(Apply(Select(New(Ident(name)),_),_),_))),_,_,Literal(Constant(rhs))) =>
          	 
          	// println("(2) value definition is matched in template or block body ...")
          	      
          	if (name.toString == "LINQAnn") {
          	    		
          	   // println("(3) LINQ annotated value definition ...")
          	   println("Value definition encapsulating a nested LINQ query is found and being visited ...")
          	    	  
          	   // parsing the corresponding string taken from right hand side part 
          	   //		of the annotated value definition 
          	   val in = new java.io.CharArrayReader (rhs.toString.toArray)
          	   val ast = sts.linq.Parser.run(in)
          	   val scala_str = sts.linq.Transformer comprehend ast
            
          	   // wrapping the resulting parse scala string to be injected 
          	   //		into a compilation unit 
               println("Scala string: " + scala_str)
          	   val unit_str = "object LINQObject{ @Persistent val " + e.name + " = " + scala_str + "; }" 
          	   unit_inject = new CompilationUnit(new BatchSourceFile("", unit_str.toCharArray))                                   
      
          	   // (1) intermidiate scalac parse phase aimed to create AST from the wrapped
          	   // 		scala string
          	   // unit_inject.body = new global.syntaxAnalyzer.UnitParser(unit_inject).parse
          	   
          	   /*
               val parser = new global.syntaxAnalyzer.UnitParser(unit_inject)
          	   unit_inject.body = parser.parse
          	   */
              
          	    
          	   object LINQToScalaParser extends ScalaQLParser(unit_inject)
          	   unit_inject.body = LINQToScalaParser.parse
          	    
          	   // tricky way to propagate a non-desugared parsing result of the injected AST node
          	   LINQToScalaParser.m.map(e => { ScalaToFerryParser.m = ScalaToFerryParser.m update (e._1, e._2.asInstanceOf[Tuple2[List[ScalaToFerryParser.treeBuilder.Enumerator], Tree]]); e})
          	   
              
               // println("extra namer is starting ...")
          	   // (2) typer phase (if typed AST is of interest)
          	   // global.analyzer.newNamer(global.analyzer.rootContext(unit_inject)).enterSym(unit_inject.body)
               // println("extra namer is finished ...")
          	   // unit_inject.body = global.analyzer.newTyper(global.analyzer.rootContext(unit_inject)).typed(unit_inject.body)
                 
          	   // println("parsing is successfully done ..." + unit_inject.body)
            
          	   // unwrapping aimed to derive the AST nodes represented the scala statements
          	   // 		generated from the initial LINQ query
          	   (unit_inject.body: @scala.unchecked) match {
          	    	case PackageDef(_, List(ModuleDef(_,_,Template(_,_,linqstats)))) =>
          	    			  
          	    	for (i <- 1 to linqstats.size-1) {
          	    		linqstats.map(stat => {resetPos.traverse(stat); atPos(e.pos)(stat)})
          	    		body1 = body1:::List(transformStats(linqstats,currentOwner)(i))
          	    	}
                
          	    	// println("(4) extracted body: " + body1)
          	    	println("scala string corresponding to the LINQ nested query: " + body1)
          
          	    	case _ => 
          	    }
          	   	
          	} else {
          	  body1 = body1:::List(tree)
          	}
            
         case _ => body1 = body1:::List(tree)
                 
       }   
     }
     body1
   }
   
   def translate () = {
     
     /*
     val parser = ScalaToFerryParser
     val unit_body = parser.parse
     */
     
     println("MAP: " + ScalaToFerryParser.m)
     Translator.TranslatorWalker.traverse(ScalaToFerryParser.unit.body)
     println("TREE: " + ScalaToFerryParser.unit.body)
   }
   
   
  }
  
  abstract class Visitor[T]() {

	import ScalaToFerryParser._
	import treeBuilder._
 
	var encodings: Map[String,T] = Map()
	var user_provided_types: Map[String, List[Tuple2[String, Option[T]]]] = Map()  
 
	def visit(e: Apply, gens: List[Tuple2[T,T]], letdecls: List[Tuple2[T,T]], filteres: List[T], rhs: T) : T
	def visit(e: Apply, qual: T, fun: String, args: List[T]) : T
	def visit(e: Apply, qual: T, fun: String, arg: Tuple2[List[T], T]) : T
	def visit(e: Apply, fun: String, args: List[T]) : T
	def visit(e: Apply, vparams : List[T], body: T, args: List[T]) : T
	def visit(e: Apply, expr: T, posAcc: Int) : T
	def visit(e: ValFrom, varid: T, bindings: List[Tuple2[T,T]], rhs: T) : Tuple2[T,T]
	def visit(e: Bind, bindings: List[Tuple2[T,T]], pat: Patterns.Value) : List[Tuple2[T,T]]
	def visit(e: Bind, bindings: List[Tuple2[T,T]], uptype: List[String]) : List[Tuple2[T,T]]                                                                            
	def visit(e: Select, qual: T, sel: String) : T
	def visit(e: Block, stats: List[Tuple2[T,T]], expr: T) : T
	def visit(e: If, e1: T, e2: T, e3: T) : T
	def visit(e: Match, casecls: List[Tuple2[List[Tuple2[T,T]], T]]) : T
	def visit(e: ValDef, name: String, exprs: List[T]) : T
	def visit(e: Ident, name: String) : T
	def visit(e: Literal, value: Any) : T
	def visit(e: Name) : T

}

class Walker[T] (v: Visitor[T], defaultVal: T) extends Traverser {

	import ScalaToFerryParser._
	import treeBuilder.{ValFrom, ValEq, Filter}
	
	// map that relates (enums, rhs) of a for comprehension with its desugared version, i.e the one with map, flatMap and filter invocations
	val forExprs = ScalaToFerryParser.m

	// traversing the whole scala AST walking some particular nodes being translated
	override def traverse(tree: Tree) = {
		tree match {
		  
		  case e @ ValDef(Modifiers(_,_,List(Annotation(Apply(Select(New(Ident(name)),_),_),_))),_,_, rhs) =>
		    if (name.toString == "Persistent") {
		      typing(walk(rhs), e)
		    }
		  
		  case e @ ValDef(_,_,_,_) =>
		    walk(e)
		  case e @ ClassDef(mods, _, _, _) =>
		    if (mods.isCase || mods.isFinal) walk(e)
		  case _ => super.traverse(tree);
		}
		
	}
 
	private def typing(ferryQ: T, tree: Tree) = {
	  if (!ferryQ.toString.contains("unsupported")) {
		  if (ferryQ.isInstanceOf[Expr]) {
		    try {
		    	val ferryQexpr = ferryQ.asInstanceOf[Expr]
		    	println("TRANSLATION (ScalaToFerry) : ") 
		    	println(ferryQexpr /* + " of " + tree */) 
		    	sts.ferry.Typer.typing(ferryQexpr)
		    	println("	TYPED with: " + ferryQexpr.tpt)
		    } catch {
		      case e: Error => global.error(e.getMessage + " in translation of " + tree)
		      // case e => global.error(e.getMessage + " in translation of " + tree)
		    }
		  } 
		  } else {
			  global.error(" translation of " + tree + " failed with the incomplete translation: " + ferryQ)
		  }
	}

	def walk(tree: Tree) : T = {
		
		tree match {
			
			case e @ Apply(se @ Select(qual, sel), args) =>

				// case of qual.map(args) or qual.flatMap(args), i.e. a desuraring of ForExpr associated with (enums, rhs) by a parser map
			  
				// TODO: in the case of patterns in generators ifrefutable check is applied to rhs
				// that rewrites rhs with filter
				if ((sel.toString=="filter")&&(args.length==1)&&args(0).toString.contains("ifrefutable"))  {
				  return walk(qual)
				}
				val forExpr = forExprs.get(tree.toString)
				forExpr match {
					// if Apply(Select(qual, sel), args) refers to a ForExpr desugaring
					case Some((enums, rhs)) =>
					  	var gens: List[Tuple2[T,T]] = List()
						var filters: List[T] = List()
						var letdecls: List[Tuple2[T,T]] = List()
						enums.map(enum =>
							{
								enum match {
									// handling a generator: pat <- rhss
									case e1 @ ValFrom(_, pat, rhss) =>
										// handling patterns
										val bindings = walkPatterns(pat, rhss)
										val varid = bindings(0)._1
										val rhss1 = bindings(0)._2
										val gen = v.visit(e1,varid,letdecls,rhss1)
										gens = gens ::: List(gen)
										val bindings1 = bindings drop 1
										if (!(bindings1).isEmpty) {
											letdecls = letdecls ::: bindings1
										}
									// handling a let declaration: val pat = rhss
									case e1 @ ValEq(_, pat, rhss) =>
										// handling patterns
										val bindings = walkPatterns(pat, rhss)
										letdecls = letdecls ::: bindings
									case e1 @ Filter(test) =>
										filters = filters ::: List(walk(test))
									case e1 @_ => defaultVal

								}
								enum
							}
						)
						val rhs1 = walk(rhs) 
						return v.visit(e: Apply, gens, letdecls, filters, rhs1)
					case None =>
				}
				
				// qual.List(...), qual.Tuple(...), qual.Set(...), user provided types
				val typeapply = walkTypeApply(e, sel.toString, args)
				if (typeapply != defaultVal) return typeapply
				
				// Operators: qual.sel(args) including the ones with arg being a closure
				// This case also includes the general Scala operators, +, -, ==, !=, etc.
				val operatorOfSubset = walkFunApply(e, qual, sel.toString, args)
				if (operatorOfSubset != defaultVal) return operatorOfSubset
    
				qual match {
					// case for new Name wrt desugaring
					// new qual.List.<init>(...), etc
					case e1 @ New(Select(_, tpt)) =>
						return walkTypeApply(e, tpt.toString, args)
					// new List.<init>(...), etc
					case e1 @ New(Ident(tpt)) =>
						return walkTypeApply(e, tpt.toString, args)
					case _ => 
				}
				
				args match {
				  case List(Literal(Constant(value))) => 
					if (value.isInstanceOf[Int]) 
						return v.visit(e, walk(se), value.asInstanceOf[Int])
					else return defaultVal
				  case _ => return defaultVal
				}
			
			// case of type instantiations List(...), Set(...), Employee(...), etc
			case e @ Apply(e1 @ Ident(name), args) => 
			  // List(...), etc
			  val typeapply = walkTypeApply(e, name.toString, args)
			  if (typeapply != defaultVal) return typeapply
			  // 	or e(num)
			  args match {
			    case List(Literal(Constant(value))) =>
			      if (value.isInstanceOf[Int]) 
						return v.visit(e, walk(e1), value.asInstanceOf[Int])
				  else return defaultVal
				case _ => return defaultVal
			  }
     
			// case of accessing List(), Set(), etc elements
			case e @ Apply(e1@Apply(_,_), List(Literal(Constant(value)))) => 
			  if (value.isInstanceOf[Int]) 
				  return v.visit(e, walk(e1), value.asInstanceOf[Int])
			  else defaultVal
     
			// case of closure invocations: ( (valdef1, ...) => expr )(arg1, ...)
			//	=> Ferry: let valdef1 = arg1, ... in expr
			case e @ Apply(Function(vparams, body), args) => 
			  val args1 = args.map(arg => walk(arg))
			  val vparam_names = vparams.map(vparam => v.visit(vparam.name))
			  val body1 = walk(body)
			  v.visit(e, vparam_names, body1, args1)
     
			// case qual.sel and no arg operators
			case e @ Select(qual, sel) =>
			  val noArgFunApply = walkFunApply(e, qual, sel.toString, List())
			  if (noArgFunApply != defaultVal) return noArgFunApply
			  v.visit(e, walk(qual), sel.toString)
			case e @ Block(stats, expr) =>
			  	stats match {
			  	  // in the case of anonymous classes
			  	  case List(stat@ClassDef(_, _, _, _)) =>
			  	    walk(stat)
			  	    walk(expr)
			  	  case _ =>
			  	    for (stat <- stats) {
			  	    	if (!stat.isInstanceOf[ValDef]) return defaultVal
			  	    }
			  	    val stats1 = stats.map(stat => stat match { case ValDef(_, name, _, rhs) => (v.visit(name), walk(rhs)) } )
			  	    val expr1 = walk(expr)
			  	    v.visit(e, stats1, expr1)
			  	}

			case e @ If(cond, thenp, elsep) =>
				v.visit(e, walk(cond), walk(thenp), walk(elsep))
			// translation of pattern matching assumes the case defs having body exprs of the 
			// 	the same type, patterns representing the same type, 
			//	and the most general case def (with the most general pattern) given at the end
			case e @ Match(selector, cases) => 
			  // TODO: unsupported quards
			  val cases1 = cases.map (c => c match {
			    					case CaseDef(pat, quard, body) => 
			    					  (walkPatterns(pat, selector), walk(body))
			  						  }
			  			 )
			  v.visit(e, cases1)
			// ClassDef instance is not translated to Ferry, but contributes to environment
			// 	carrying supported user defined types
			case e @ ClassDef(_, name, _, Template(_, _, body)) =>
			  val body1 = body.filter(stat => stat.isInstanceOf[ValDef])
			  		.map(stat => stat match { case ValDef(_, name, _, rhs) => 
			  		  // in the case of final class: new {...} fields' values are in class def
			  		  if (rhs == EmptyTree) 
			  			  (name.toString, None)
			  		  else
			  		  (name.toString, Some(walk(rhs)))})
			  v.user_provided_types = v.user_provided_types update (name.toString, body1)
			  // println("user_provided_types: " + v.user_provided_types)
			  v.visit("new user provided type is added")
     
			// ValDef instance is not translated to Ferry, but contributes to environment carrying encodings
			case e @ ValDef(_, name, _, rhs) =>
			  	rhs match {
			  	  case EmptyTree => return v.visit("val def with no rhs is found")
			  	  case _ =>
			  	    val name1 = v.visit(name)
			  	    rhs match {
			  	    	case Match(_,_) => walkValDefPatterns(e, rhs); return v.visit("new val def is added")
			  	    	case _ =>                                   
			  	    }
			  	    val rhs1 = walk(rhs)
			  	    v.encodings.get(rhs.toString) match {
			  	    	case Some(rhs2) => v.encodings = v.encodings update (name1.toString, rhs2)
			  	    	case None => v.encodings = v.encodings update (name1.toString, rhs1)
			  	    }
			  	    // println("encodings: " + v.encodings)
			  	    return v.visit("new val def is added")
			  	}
			
			case e @ Ident(name) => v.visit(e, name.toString)
			// remove to the approprite constucts to provide the context where it can occur
			case e @ Literal(Constant(value)) => v.visit(e, value)
			
			case _ => defaultVal
				
		}
		
	}
	
	def walkPatterns(tree: Tree, rhs: Tree) : List[Tuple2[T,T]] = {
		// supported: v @ ( x1@_, v1 @ (x2, x3), x4 ) and v @ List( x1@_, v1 @ List(x2, x3), x4 )
		// TODO: ( x1@_, (x2, x3), x4 ) and List( x1@_, List(x2, x3), x4 )

		val rhs1 = walk(rhs)
		walkPatterns(tree, rhs1)

	}

	def walkPatterns(tree: Tree, rhs: T) : List[Tuple2[T,T]] = {
		tree match {
			case e2 @ Bind(name, Apply(Ident(name1), args)) =>
				// supported: lists in patterns with outer binding x @ List(...)
				if (name1.toString.contains("List")) {
					val args1 = args.flatMap(arg => walkPatterns(arg, Ident(name)))
					return v.visit(e2, List(Tuple2(v.visit(name), rhs)) ::: args1, Patterns.ListPat)
				}
				// supported: user-defined types
				v.user_provided_types.get(name1.toString) match {
				  case Some(uptype) =>
				    val args1 = args.flatMap(arg => walkPatterns(arg, Ident(name)))
				    return v.visit(e2, List(Tuple2(v.visit(name), rhs)) ::: args1, uptype.map(e => e._1))
				  case None =>
				}
				List((defaultVal, defaultVal))
			case e2 @ Bind(name, Apply(Select(_, name1), args)) =>
				// supported: tuples in patterns with outer binding x @ (...)
				if (name1.toString.contains("Tuple")) {
					val args1 = args.flatMap(arg => walkPatterns(arg, Ident(name)))
					return v.visit(e2, List(Tuple2(v.visit(name), rhs)) ::: args1, Patterns.TuplePat)
				}
				// supported: user-defined types
				v.user_provided_types.get(name1.toString) match {
				  case Some(uptype) =>
				    val args1 = args.flatMap(arg => walkPatterns(arg, Ident(name)))
				    return v.visit(e2, List(Tuple2(v.visit(name), rhs)) ::: args1, uptype.map(e => e._1))
				  case None =>
				}
				List((defaultVal, defaultVal))
			
			case e2 @ Apply(Ident(name1), args) =>
				// supported: lists and user-provided types in patterns without outer binding List(x @ _, ...)
			  
				// wrapping in Bind node: fv$seq @ List(x @ _, ...)
				walkPatterns(Bind(ScalaToFerryParser.freshName(scala.tools.nsc.util.NoPosition, "fv$"), e2), rhs)
				
			case e2 @ Apply(Select(_, name1), args) =>
				// supported: tuples and user-provided types in patterns without outer binding (x @ _, ...)
			  
				// wrapping in Bind node: fv$seq @ (x @ _, ...)
				walkPatterns(Bind(ScalaToFerryParser.freshName(scala.tools.nsc.util.NoPosition, "fv$"), e2), rhs)
				
			// TODO: other types in patterns

			case e2 @ Bind(name, Ident(name1)) =>
				// supported: pattern name @ _
				if (name1.toString == "_") {
					return List(Tuple2(v.visit(name), rhs))
				}
				List((defaultVal, defaultVal))
			case e2 @ Ident(name) =>
				// supported: _
				if (name.toString == "_") {
					return List(Tuple2(v.visit(name), rhs))
				}
				List((defaultVal, defaultVal))
			case e2 @ Star(Ident(name)) =>
				// supported: _*
				if (name.toString == "_") {
					return List(Tuple2(v.visit(name), rhs))
				}
				List((defaultVal, defaultVal))
    
			// Literal may occur in the pattern matching of case clauses
			case e2 @ Literal(Constant(value)) => return List(Tuple2(v.visit(e2, value), rhs))

		}
	}
	def walkValDefPatterns(tree: ValDef, expr: Tree) = {
	  expr match {
	    case Match(Annotated(_,selector), cases) =>
	      cases(0).pat match {
	        case Bind(name,_) => v.encodings = v.encodings update (name.toString, walk(selector))
	      }
	      val bindings = walkPatterns(cases(0).pat, selector)
	      v.encodings = v.encodings update (tree.name.toString, v.visit(tree, tree.name.toString, bindings.map(binding => binding._2)))
	      // println("encodings: " + v.encodings)
	  }
	}
 
	def walkTypeApply(tree: Apply, tpt: String, args: List[Tree]): T = {
	  tpt match {
			case "List" | "Set" => 
				return v.visit(tree, tpt.toString, walk(args))
			case _ =>
				 if (tpt.contains("Tuple")) 
					return v.visit(tree, tpt, walk(args))
				 v.user_provided_types.get(tpt) match {
					case Some(tpt1) => return v.visit(tree, tpt, walk(args))
					case None => return defaultVal
				 }
	  } 
	}
	def walkFunApply(tree: Tree, qual: Tree, fun: String, args: List[Tree]): T = {
	  fun match {
	    case "map" | "filter" | "filterNot" | "flatMap" | "partition" | "forall" | "exists" | "count" | "takeWhile" | "dropWhile" | "span" => 
	      args match {
	        // case of a closure as an arg
	        case List(Function(vparams, body)) => 
	        	val vparam_names = vparams.map(vparam => v.visit(vparam.name))
	        	v.visit(tree.asInstanceOf[Apply], walk(qual), fun, (vparam_names, walk(body)))
	        case _ => defaultVal
	      }
	    // TODO: what about x == (1,2) that is parsed as x.$eq$eq(1,2), 
	    //				i.e. with two args instead of one (1,2)
	    case "drop" | "take" | "splitAt" | "takeRight" | "takeLeft" =>
	      args match {
	        case List(Literal(Constant(value))) =>
	          if (value.isInstanceOf[Int]) 
	        	  return v.visit(tree.asInstanceOf[Apply], walk(qual), fun, walk(args))
	          else {
	           return defaultVal
	          } 
	        case List(Ident(name)) => 
	        	return v.visit(tree.asInstanceOf[Apply], walk(qual), fun, walk(args))
	        case _ => return defaultVal
	      }
	    case "slice" =>
	      args match {
	        case List(arg1, arg2) =>
	          args.map(_ match { 
	            case Literal(Constant(value)) =>
	            case Ident(name) =>
	            case _ => return defaultVal
	          })
	          return v.visit(tree.asInstanceOf[Apply], walk(qual), fun, walk(args))
	        case _ => defaultVal
	      }
	    case "zip" | "sameElements" | "contains" =>
	    	args match {
	    		case List(arg) => return v.visit(tree.asInstanceOf[Apply], walk(qual), fun, walk(args))
	    		case _ => return defaultVal
	    	}
      
	      
	    // standard scala operators
	    case "$eq$eq" | "$bang$eq" | "$greater" | "$greater$eq" | "$less" | "$less$eq" | "$plus" | "$minus" | "$times" | "$div" => 
	      v.visit(tree.asInstanceOf[Apply], walk(qual), fun, walk(args))
       
	    case "length" => v.visit(tree.asInstanceOf[Select], walk(qual), fun)
	    case _ => defaultVal
	    
	  }
	}

	def walk(args: List[Tree]) : List[T] = {
	  args.map(arg => walk(arg))
	}
 
}

object Patterns extends Enumeration {
	val ListPat = Value("List")
	val TuplePat = Value("Tuple")
}

object Translator extends Visitor[FerryAttr]() {

	import ScalaToFerryParser.treeBuilder._

	type T = FerryAttr
 
	val tables: Map[String, List[Tuple2[TableColID, Fa]]] = 
		Map("Employees" -> List((TableColID("id"), Fa(Atom.Fint)),
                          (TableColID("name"), Fa(Atom.Fstring)),
                          (TableColID("dept"), Fa(Atom.Fstring)),
                          (TableColID("salary"), Fa(Atom.Fint))));

	override def visit(e: Apply, gens: List[Tuple2[T,T]], letdecls: List[Tuple2[T,T]], filters: List[T], rhs: T) : T = {
		ForExpr(ForBindingClause(gens.map(gen => (gen._1.asInstanceOf[VarIDDecl], gen._2.asInstanceOf[Expr]))),
				if (filters.isEmpty) { None } else { 
					val filterAll =  filters.reduceLeft((filter1, filter2) => BinOpExpr(filter1.asInstanceOf[Expr], filter2.asInstanceOf[Expr], BinOp.FAnd))
					if (letdecls.isEmpty) { Some(WhereClause(filterAll.asInstanceOf[Expr])) } else {
						Some(WhereClause(LetExpr( LetBindingClause(letdecls.map(decl => (decl._1.asInstanceOf[VarIDDecl], decl._2.asInstanceOf[Expr]))), filterAll.asInstanceOf[Expr]))) 
					}
				}, 
				None, None, None,
				if (letdecls.isEmpty) { rhs.asInstanceOf[Expr] } else {
					LetExpr( LetBindingClause(letdecls.map(decl => (decl._1.asInstanceOf[VarIDDecl], decl._2.asInstanceOf[Expr]))), rhs.asInstanceOf[Expr]) 
				}
				) 
	}
	
	// operators with a list of args
	override def visit(e: Apply, qual: T, fun: String, args: List[T]) : T = {
		
		fun match {
	    	
			// standard Scala operators
	    	case "$eq$eq" =>
	    		if (args.length == 1)
	    			return BinOpExpr(qual.asInstanceOf[Expr], args(0).asInstanceOf[Expr], BinOp.Feq)
	    		else
	    			// expression: x.$eq$eq(1,2,...) is interpreted as x.$eq$eq( (1,2,...) )
	    			return BinOpExpr(qual.asInstanceOf[Expr], TupleExpr(args.map(arg => arg.asInstanceOf[Expr])), BinOp.Feq)
	    	case "$bang$eq" =>
	    		if (args.length == 1)
	    			return BinOpExpr(qual.asInstanceOf[Expr], args(0).asInstanceOf[Expr], BinOp.Fnq)
	    		else
	    			// expression: x.$eq$eq(1,2,...) is interpreted as x.$eq$eq( (1,2,...) )
	    			return BinOpExpr(qual.asInstanceOf[Expr], TupleExpr(args.map(arg => arg.asInstanceOf[Expr])), BinOp.Fnq)
	    	
	    	case "$greater" =>
	    	  if (args.length == 1)
	    			return BinOpExpr(qual.asInstanceOf[Expr], args(0).asInstanceOf[Expr], BinOp.Fgt)
	    		else
	    			// expression: x.$greater(1,2,...) is interpreted as x.$greater( (1,2,...) )
	    			return BinOpExpr(qual.asInstanceOf[Expr], TupleExpr(args.map(arg => arg.asInstanceOf[Expr])), BinOp.Fgt)
	    	case "$greater$eq" =>
	    	  if (args.length == 1)
	    			return BinOpExpr(qual.asInstanceOf[Expr], args(0).asInstanceOf[Expr], BinOp.Fge)
	    		else
	    			// expression: x.$greater$eq(1,2,...) is interpreted as x.$greater$eq( (1,2,...) )
	    			return BinOpExpr(qual.asInstanceOf[Expr], TupleExpr(args.map(arg => arg.asInstanceOf[Expr])), BinOp.Fge)
	    	case "$less" =>
	    	  if (args.length == 1)
	    			return BinOpExpr(qual.asInstanceOf[Expr], args(0).asInstanceOf[Expr], BinOp.Flt)
	    		else
	    			// expression: x.$less(1,2,...) is interpreted as x.$less( (1,2,...) )
	    			return BinOpExpr(qual.asInstanceOf[Expr], TupleExpr(args.map(arg => arg.asInstanceOf[Expr])), BinOp.Flt)
	    	case "$less$eq" =>
	    	  if (args.length == 1)
	    			return BinOpExpr(qual.asInstanceOf[Expr], args(0).asInstanceOf[Expr], BinOp.Fle)
	    		else
	    			// expression: x.$less$eq(1,2,...) is interpreted as x.$less$eq( (1,2,...) )
	    			return BinOpExpr(qual.asInstanceOf[Expr], TupleExpr(args.map(arg => arg.asInstanceOf[Expr])), BinOp.Fle)
            
	    	case "$plus" =>
	    	  if (args.length == 1)
	    			return BinOpExpr(qual.asInstanceOf[Expr], args(0).asInstanceOf[Expr], BinOp.Fadd)
	    		else
	    			StringLiteral("unsupported number of args for + operator")
	    	case "$minus" =>
	    	  if (args.length == 1)
	    			return BinOpExpr(qual.asInstanceOf[Expr], args(0).asInstanceOf[Expr], BinOp.Fminus)
	    		else
	    			StringLiteral("unsupported number of args for - operator")
	    	
	    	case "$times" =>
	    	  if (args.length == 1)
	    			return BinOpExpr(qual.asInstanceOf[Expr], args(0).asInstanceOf[Expr], BinOp.Fmult)
	    		else
	    			StringLiteral("unsupported number of args for * operator")
	    	case "$div" =>
	    	  if (args.length == 1)
	    			return BinOpExpr(qual.asInstanceOf[Expr], args(0).asInstanceOf[Expr], BinOp.Fdiv)
	    		else
	    			StringLiteral("unsupported number of args for / operator")
        
        
	    	// Scala collection operators
	    	case "take" => 
	    	  	FunAppExpr(FunID(FunIDs.Ftake), FunListArgs(List(args(0).asInstanceOf[IntLiteral], qual.asInstanceOf[Expr])))
	    	case "drop" =>
	    		FunAppExpr(FunID(FunIDs.Fdrop), FunListArgs(List(args(0).asInstanceOf[IntLiteral], qual.asInstanceOf[Expr])))
	    	case "zip" =>
	    		FunAppExpr(FunID(FunIDs.Fzip), FunListArgs(List(qual.asInstanceOf[Expr], args(0).asInstanceOf[Expr])))
	    	case "splitAt" =>
	    	  TupleExpr(List(FunAppExpr(FunID(FunIDs.Ftake), FunListArgs(List(args(0).asInstanceOf[IntLiteral], qual.asInstanceOf[Expr]))), FunAppExpr(FunID(FunIDs.Fdrop), FunListArgs(List(args(0).asInstanceOf[IntLiteral], qual.asInstanceOf[Expr])))))
	    	case "slice" =>
	    	  FunAppExpr(FunID(FunIDs.Fmap), FunCondArgs(VarIDDecl("v"), FunAppExpr(FunID(FunIDs.Fnth), FunListArgs(List(VarUseExpr(VarIDDecl("v")), qual.asInstanceOf[Expr]))), FunAppExpr(FunID(FunIDs.Frange), FunListArgs(List(IntLiteral(args(0).asInstanceOf[IntLiteral].literal + 1), IntLiteral(args(1).asInstanceOf[IntLiteral].literal + 1))))))
	    	case "takeRight" =>
	    	  FunAppExpr(FunID(FunIDs.Fdrop), FunListArgs(List(BinOpExpr(FunAppExpr(FunID(FunIDs.Flength), FunListArgs(List(qual.asInstanceOf[Expr]))), args(0).asInstanceOf[IntLiteral], BinOp.Fminus), qual.asInstanceOf[Expr])))
	    	case "takeLeft" =>
	    	  FunAppExpr(FunID(FunIDs.Ftake), FunListArgs(List(args(0).asInstanceOf[IntLiteral], qual.asInstanceOf[Expr])))
	    	case "sameElements" =>
	    	  BinOpExpr(BinOpExpr(FunAppExpr(FunID(FunIDs.Flength), FunListArgs(List(qual.asInstanceOf[Expr]))), FunAppExpr(FunID(FunIDs.Flength), FunListArgs(List(args(0).asInstanceOf[Expr]))), BinOp.Feq), LetExpr(LetBindingClause(List((VarIDDecl("s'"), FunAppExpr(FunID(FunIDs.Ffilter), FunCondArgs(VarIDDecl("v"), BinOpExpr(PosAccExpr(VarUseExpr(VarIDDecl("v")), NatLiteral(1)), PosAccExpr(VarUseExpr(VarIDDecl("v")), NatLiteral(2)), BinOp.Fnq), FunAppExpr(FunID(FunIDs.Fzip), FunListArgs(List(qual.asInstanceOf[Expr], args(0).asInstanceOf[Expr])))))))), BinOpExpr(FunAppExpr(FunID(FunIDs.Flength), FunListArgs(List(VarUseExpr(VarIDDecl("s'"))))), IntLiteral(0), BinOp.Feq)), BinOp.FAnd)
        
	    	case "contains" => BinOpExpr(FunAppExpr(FunID(FunIDs.Flength), FunListArgs(List(FunAppExpr(FunID(FunIDs.Ffilter), FunCondArgs(VarIDDecl("v"), BinOpExpr(VarUseExpr(VarIDDecl("v")), args(0).asInstanceOf[Expr], BinOp.Feq), qual.asInstanceOf[Expr]))))), IntLiteral(0), BinOp.Fnq)
        
	    	case _ => StringLiteral("unsupported operators with list of args")
	    
		}

	}
 
	// operators with an arg being a closure
	override def visit(e: Apply, qual: T, fun: String, arg: Tuple2[List[T], T]) : T = {
		fun match {
		  case "map" => FunAppExpr(FunID(FunIDs.Fmap), 
                             FunCondArgs(arg._1(0).asInstanceOf[VarIDDecl], 
                                         arg._2.asInstanceOf[Expr],
                             			 qual.asInstanceOf[Expr]))
		  case "filter" => FunAppExpr(FunID(FunIDs.Ffilter), 
                             FunCondArgs(arg._1(0).asInstanceOf[VarIDDecl], 
                                         arg._2.asInstanceOf[Expr],
                             			 qual.asInstanceOf[Expr]))
		  case "filterNot" => FunAppExpr(FunID(FunIDs.Ffilter), 
                             	FunCondArgs(arg._1(0).asInstanceOf[VarIDDecl], 
                                         UnOpExpr(arg._2.asInstanceOf[Expr], UnOp.FNot),
                             			 qual.asInstanceOf[Expr]))
		  case "flatMap" => FunAppExpr(FunID(FunIDs.Fconcat), FunListArgs(List(FunAppExpr(FunID(FunIDs.Fmap), 
                             FunCondArgs(arg._1(0).asInstanceOf[VarIDDecl], 
                                         arg._2.asInstanceOf[Expr],
                             			 qual.asInstanceOf[Expr]))))) 
		  case "partition" => TupleExpr(List(FunAppExpr(FunID(FunIDs.Ffilter), 
                             					FunCondArgs(arg._1(0).asInstanceOf[VarIDDecl], 
                             							arg._2.asInstanceOf[Expr],
                             							qual.asInstanceOf[Expr])), 
                             				 FunAppExpr(FunID(FunIDs.Ffilter), 
                             					FunCondArgs(arg._1(0).asInstanceOf[VarIDDecl], 
                             							UnOpExpr(arg._2.asInstanceOf[Expr], UnOp.FNot),
                             							qual.asInstanceOf[Expr]))))
		  case "forall" => BinOpExpr(FunAppExpr(FunID(FunIDs.Flength), FunListArgs(List(FunAppExpr(FunID(FunIDs.Ffilter), 
                             					FunCondArgs(arg._1(0).asInstanceOf[VarIDDecl], 
                             							UnOpExpr(arg._2.asInstanceOf[Expr], UnOp.FNot),
                             							qual.asInstanceOf[Expr]))))), IntLiteral(0), BinOp.Feq)
		    
		  case "exists" => BinOpExpr(FunAppExpr(FunID(FunIDs.Flength), FunListArgs(List(FunAppExpr(FunID(FunIDs.Ffilter), 
                             					FunCondArgs(arg._1(0).asInstanceOf[VarIDDecl], 
                             							arg._2.asInstanceOf[Expr],
                             							qual.asInstanceOf[Expr]))))), IntLiteral(0), BinOp.Fnq)
		    
		  case "count" => FunAppExpr(FunID(FunIDs.Flength), FunListArgs(List(FunAppExpr(FunID(FunIDs.Ffilter), 
                             					FunCondArgs(arg._1(0).asInstanceOf[VarIDDecl], 
                             							arg._2.asInstanceOf[Expr],
                             							qual.asInstanceOf[Expr])))))
		    
		  case "takeWhile" => LetExpr(LetBindingClause(List((VarIDDecl("prefixs"), FunAppExpr(FunID(FunIDs.Ffilter), FunCondArgs(VarIDDecl("v"), BinOpExpr(PosAccExpr(VarUseExpr(VarIDDecl("v")), NatLiteral(1)), IntLiteral(0), BinOp.Fnq), FunAppExpr(FunID(FunIDs.Fmap), FunCondArgs(VarIDDecl("v"), LetExpr( LetBindingClause(List( (VarIDDecl("sPrefix"), FunAppExpr(FunID(FunIDs.Ftake), FunListArgs(List(VarUseExpr(VarIDDecl("v")), qual.asInstanceOf[Expr])))))), IfExpr( BinOpExpr(FunAppExpr(FunID(FunIDs.Flength), FunListArgs(List( FunAppExpr(FunID(FunIDs.Ffilter), FunCondArgs(arg._1(0).asInstanceOf[VarIDDecl], arg._2.asInstanceOf[Expr], VarUseExpr(VarIDDecl("sPrefix"))))))), FunAppExpr(FunID(FunIDs.Flength), FunListArgs(List(VarUseExpr(VarIDDecl("sPrefix"))))), BinOp.Feq), TupleExpr(List(FunAppExpr(FunID(FunIDs.Flength), FunListArgs(List(VarUseExpr(VarIDDecl("sPrefix"))))), VarUseExpr(VarIDDecl("v")))), TupleExpr(List(IntLiteral(0), VarUseExpr(VarIDDecl("v")))))), FunAppExpr(FunID(FunIDs.Frange), FunListArgs(List(IntLiteral(1), FunAppExpr(FunID(FunIDs.Flength), FunListArgs(List(qual.asInstanceOf[Expr]))))))))))))), FunAppExpr(FunID(FunIDs.Ftake), FunListArgs(List(PosAccExpr(FunAppExpr(FunID(FunIDs.Fnth), FunListArgs(List(FunAppExpr(FunID(FunIDs.Flength), FunListArgs(List(VarUseExpr(VarIDDecl("prefixs"))))), VarUseExpr(VarIDDecl("prefixs"))))), NatLiteral(2)), qual.asInstanceOf[Expr]))))
		    
		  case "dropWhile" => LetExpr(LetBindingClause(List((VarIDDecl("prefixs"), FunAppExpr(FunID(FunIDs.Ffilter), FunCondArgs(VarIDDecl("v"), BinOpExpr(PosAccExpr(VarUseExpr(VarIDDecl("v")), NatLiteral(1)), IntLiteral(0), BinOp.Fnq), FunAppExpr(FunID(FunIDs.Fmap), FunCondArgs(VarIDDecl("v"), LetExpr( LetBindingClause(List( (VarIDDecl("sPrefix"), FunAppExpr(FunID(FunIDs.Ftake), FunListArgs(List(VarUseExpr(VarIDDecl("v")), qual.asInstanceOf[Expr])))))), IfExpr( BinOpExpr(FunAppExpr(FunID(FunIDs.Flength), FunListArgs(List( FunAppExpr(FunID(FunIDs.Ffilter), FunCondArgs(arg._1(0).asInstanceOf[VarIDDecl], UnOpExpr(arg._2.asInstanceOf[Expr], UnOp.FNot), VarUseExpr(VarIDDecl("sPrefix"))))))), FunAppExpr(FunID(FunIDs.Flength), FunListArgs(List(VarUseExpr(VarIDDecl("sPrefix"))))), BinOp.Feq), TupleExpr(List(FunAppExpr(FunID(FunIDs.Flength), FunListArgs(List(VarUseExpr(VarIDDecl("sPrefix"))))), VarUseExpr(VarIDDecl("v")))), TupleExpr(List(IntLiteral(0), VarUseExpr(VarIDDecl("v")))))), FunAppExpr(FunID(FunIDs.Frange), FunListArgs(List(IntLiteral(1), FunAppExpr(FunID(FunIDs.Flength), FunListArgs(List(qual.asInstanceOf[Expr]))))))))))))), FunAppExpr(FunID(FunIDs.Ftake), FunListArgs(List(PosAccExpr(FunAppExpr(FunID(FunIDs.Fnth), FunListArgs(List(FunAppExpr(FunID(FunIDs.Flength), FunListArgs(List(VarUseExpr(VarIDDecl("prefixs"))))), VarUseExpr(VarIDDecl("prefixs"))))), NatLiteral(2)), qual.asInstanceOf[Expr]))))
		    
		  case "span" => TupleExpr(List(visit(e, qual, "takeWhile", arg).asInstanceOf[Expr], visit(e, qual, "dropWhile", arg).asInstanceOf[Expr]))
		    
		  case _ => StringLiteral("unsupported operator with an arg being a closure")
		}
	} 
	
	// qual.Tuple$seq(...), [qual].List(...), [qual].Set(...), user provided types
	override def visit(e: Apply, fun: String, args: List[T]) : T = {
	  
		fun match {
			case "List" | "Set" => 
				return ListExpr(args.map(arg => arg.asInstanceOf[Expr]))
			case _ =>
				 if (fun.contains("Tuple")) 
					return TupleExpr(args.map(arg => arg.asInstanceOf[Expr]))
				 user_provided_types.get(fun) match {
				 	case Some(uptype) => 
				 		if (args.isEmpty) 
				 				return RecordExpr(uptype.map(t => TableColID(t._1)), 
				 							uptype.map(t => t._2 match { 
				 								case Some(expr) => expr.asInstanceOf[Expr]
				 								case None => StringLiteral("unsupported type")                                    
				 							} 
				 						))
				 		else
				 			return RecordExpr(uptype.map(t => TableColID(t._1)), args.map(arg => arg.asInstanceOf[Expr]))
	        
				 	case None =>  return StringLiteral("unsupported type")
				 } 
	    }
	}
	
	override def visit(e: Apply, expr: T, posAcc: Int) : T = {
	  return FunAppExpr(FunID(FunIDs.Fnth), FunListArgs(List(IntLiteral(posAcc + 1), expr.asInstanceOf[Expr])))
	}
 
	override def visit(e: Apply, vparams : List[T], body: T, args: List[T]) : T = {
	  val bindings = vparams zip args
	  LetExpr(LetBindingClause(bindings.map(binding => 
	    				(binding._1.asInstanceOf[VarIDDecl], binding._2.asInstanceOf[Expr]))), 
           body.asInstanceOf[Expr])
	} 
 
	override def visit(e: ValFrom, varid: T, bindings: List[Tuple2[T,T]], rhs: T) : Tuple2[T,T] = {
		if (bindings.isEmpty) {
			(varid, rhs)
		} else {
			(varid, LetExpr(LetBindingClause(bindings.map(binding => (binding._1.asInstanceOf[VarIDDecl], binding._2.asInstanceOf[Expr]))), rhs.asInstanceOf[Expr]))
		}
	}

	override def visit(e: Bind, bindings: List[Tuple2[T,T]], pat: Patterns.Value) : List[Tuple2[T,T]] = {
		pat match  {
			case Patterns.ListPat =>
				val bindings1 = for (binding <- (bindings drop 1)) yield 
					(binding._1, FunAppExpr(FunID(FunIDs.Fnth), FunListArgs(List(IntLiteral(bindings indexOf binding), binding._2.asInstanceOf[Expr]))))
				val bindings2 = bindings1.filter(binding => binding._1 match {case VarIDDecl("_") => false; case _ => true})
				List(bindings(0)):::bindings2
				
			case Patterns.TuplePat =>
				val bindings1 = for (binding <- (bindings drop 1)) yield 
					(binding._1, PosAccExpr(binding._2.asInstanceOf[Expr], NatLiteral(bindings indexOf binding)))
				val bindings2 = bindings1.filter(binding => binding._1 match {case VarIDDecl("_") => false; case _ => true})
				List(bindings(0)):::bindings2

			case _ => List((StringLiteral("unsupported pattern"), StringLiteral("unsupported pattern")))
		}
	}
	
	override def visit(e: Bind, bindings: List[Tuple2[T,T]], uptype: List[String]) : List[Tuple2[T,T]] = {
		var pos = -1
		val bindings1 = for (binding <- (bindings drop 1)) yield {
			pos = pos + 1
			(binding._1, NomAccExpr(binding._2.asInstanceOf[Expr], TableColID(uptype(pos))))
			}
		val bindings2 = bindings1.filter(binding => binding._1 match {case VarIDDecl("_") => false; case _ => true})
		List(bindings(0)):::bindings2
	}

	override def visit(e: Select, qual: T, sel: String) : T = {
		if (sel.contains("_")) 
			// tuple elements accessing
			return PosAccExpr(qual.asInstanceOf[Expr], NatLiteral(sel.substring(1,sel.length).toInt))

		// no arg operators
		sel match {
		  case "length" | "size" => return FunAppExpr(FunID(FunIDs.Flength), FunListArgs(List(qual.asInstanceOf[Expr])))
		  case "isEmpty" => return BinOpExpr(FunAppExpr(FunID(FunIDs.Flength), FunListArgs(List(qual.asInstanceOf[Expr]))), IntLiteral(0), BinOp.Feq)
		  case "nonEmpty" => return BinOpExpr(FunAppExpr(FunID(FunIDs.Flength), FunListArgs(List(qual.asInstanceOf[Expr]))), IntLiteral(0), BinOp.Fnq)
		  case "head" => return FunAppExpr(FunID(FunIDs.Fnth), FunListArgs(List(IntLiteral(1), qual.asInstanceOf[Expr])))
		  case "tail" => return FunAppExpr(FunID(FunIDs.Fdrop), FunListArgs(List(IntLiteral(1), qual.asInstanceOf[Expr])))
		  case "last" => return FunAppExpr(FunID(FunIDs.Fnth), FunListArgs(List(FunAppExpr(FunID(FunIDs.Flength), FunListArgs(List(qual.asInstanceOf[Expr]))), qual.asInstanceOf[Expr])))
		  case "init" => return FunAppExpr(FunID(FunIDs.Ftake), FunListArgs(List(BinOpExpr(FunAppExpr(FunID(FunIDs.Flength), FunListArgs(List(qual.asInstanceOf[Expr]))), IntLiteral(1), BinOp.Fminus), qual.asInstanceOf[Expr])))
		  case _ => 
		}
		NomAccExpr(qual.asInstanceOf[Expr], TableColID(sel))
		
	}

	override def visit(e: Block, stats: List[Tuple2[T,T]], expr: T) : T = {
		LetExpr(LetBindingClause(stats.map(stat => (stat._1.asInstanceOf[VarIDDecl], stat._2.asInstanceOf[Expr]))), expr.asInstanceOf[Expr])
	}

	override def visit(e: If, e1: T, e2: T, e3: T) : T = {
		IfExpr(e1.asInstanceOf[Expr], e2.asInstanceOf[Expr], e3.asInstanceOf[Expr])
	}
 
	override def visit(e: Match, casecls: List[Tuple2[List[Tuple2[T,T]], T]]) : T = {
	  val casecls1 = casecls.map(cls =>
	    	{
	    	  var bindings: List[Tuple2[VarIDDecl, Expr]] = List()
	    	  var conds: List[Tuple2[Expr, Expr]] = List()
	    	  cls._1.map(c => 
	    	    c._1 match {
	    	    	case VarIDDecl(id) => 
	    	    	if (id != "_") bindings = bindings ::: List((c._1.asInstanceOf[VarIDDecl], c._2.asInstanceOf[Expr]))
	    	    	case cond => conds = conds ::: List((c._1.asInstanceOf[Expr], c._2.asInstanceOf[Expr]))
	    	    }
	    	   )
	    	   (conds, bindings, cls._2.asInstanceOf[Expr])
	    	} 
	  	)
	  // => if conds then let bindings in rhs else ...
	  val casecls1reverse = casecls1.reverse
	  val innermost = casecls1reverse(0)
	  // innermost is assumed to be the most general case of matching implying no conditions
	  var elseExpr = if (innermost._2.isEmpty) { 
	    innermost._3 
	  } else {
	    LetExpr(LetBindingClause(innermost._2), innermost._3)
	  }
	  (casecls1reverse drop 1).map(cls =>
	    // case defs before the most general ones are expected to have literals in their patterns
	    //	otherwise the compiler error of code unreachability
       {
        var condAll = cls._1.map(cond => BinOpExpr(cond._2,cond._1,BinOp.Feq))
          .reduceLeft((e1,e2)=> BinOpExpr(e1,e2,BinOp.FAnd))
        elseExpr = IfExpr(if (cls._2.isEmpty) {condAll} else {LetExpr(LetBindingClause(cls._2),condAll)},
                       if (cls._2.isEmpty) {cls._3} else {LetExpr(LetBindingClause(cls._2),cls._3)}, 
                       elseExpr)
	   }
	  )
	  val finalIfExpr = elseExpr
	  finalIfExpr
	}

	override def visit(e: ValDef, name: String, exprs: List[T]) : T = {
	  TupleExpr(exprs.map(expr => expr.asInstanceOf[Expr]))
	}
 
	override def visit(e: Ident, name: String) : T = {
		encodings.get(name) match {
			case Some(encoding) => encoding
			case None => 
			  tables.get(name) match {
			    case Some(table) => 
			      TableRefExpr(TableID(name), TableColSpecs(table.map(spec => TableColSpec(spec._1, spec._2.elemT))), TableKeySpecs(List(TableKeySpec(List(table(0)._1)))), None) 
			    case None => VarUseExpr(VarIDDecl(name))
			  } 
		}
	}

	override def visit(e: Literal, value: Any) : T = {
		if (value.isInstanceOf[Int]) return IntLiteral(value.asInstanceOf[Int])
		if (value.isInstanceOf[Double]) return IntLiteral(value.asInstanceOf[Int])
		if (value.isInstanceOf[String]) return StringLiteral(value.asInstanceOf[String])
		if (value.isInstanceOf[Boolean]) return BoolLiteral(value.asInstanceOf[Boolean])
		StringLiteral("unsupported by ScalaQLTranslator")
	}

	override def visit(e: Name) : T = {
		VarIDDecl(e.toString)
	}

	object TranslatorWalker extends Walker[FerryAttr] (this, StringLiteral("unsupported by ScalaQLTranslator"))

}

  class ScalaQLParser(unit: CompilationUnit) extends global.syntaxAnalyzer.UnitParser(unit) {
    
    	 		 import scala.tools.nsc.ast.parser.Tokens._
                 import treeBuilder._
                 
                 var m: Map[String,Tuple2[List[Enumerator], Tree]] = Map()
         
                 override def expr(): Tree = {
                   
                   in.token match {
                   		case scala.tools.nsc.ast.parser.Tokens.FOR => 
                   		atPos(in.skipToken) {
                   			val startToken = in.token
                   			val (open,close) = if (startToken == LBRACE) (LBRACE,RBRACE) else (LPAREN,RPAREN)
                   			val enums = surround(open,close)(enumerators(), Nil)
                   			newLinesOpt()
                   			if (in.token == YIELD) {
                   				in.nextToken;
                   				val rhs = super.expr()
                   				val tree = makeForYield(enums, rhs)
                   				m = m update(tree.toString, (enums, rhs))
                   				tree
                   			} else makeFor(enums, super.expr())
                        
                   		}
                   		case _ => super.expr()
                   	}
                 }
                 
               }
  
  object ScalaToFerryParser extends ScalaQLParser(cur_unit)
  
}
