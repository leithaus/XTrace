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


package sts.ferry

object TyperVisitor extends sts.ferry.AbstractSyntax.Visitor[sts.ferry.AbstractSyntax.FerryAttr] {
  
   import AbstractSyntax._
   
   type T = FerryAttr
  
   override def visit(e: IntLiteral, literal: Int) = {
    e
   }
   override def visit(e: StringLiteral, literal: String) = {
     e
   }
   override def visit(e: BoolLiteral, literal: Boolean) = {
     e
   }
   override def visit(e: UnOpExpr, expr: T, op: UnOp.Value) = {
     
     val expr1 =  expr.asInstanceOf[Expr]
     
     if (!OfFa.check(expr1.tpt)) throw new Error("ERROR: Type error when calculating a type for " + e + " expression")
     
     if (Set(UnOp.FNot) contains op) {
          if (expr1.tpt != Fa(Atom.Fbool)) {
            throw new Error("ERROR: Type error when calculating a type for " + e + " expression")
          } else {
            e.tpt_=(Fa(Atom.Fbool))
          }
     } 
     e
   } 
   override def visit(e: BinOpExpr, lhs: T, rhs: T, op: BinOp.Value): T = {
     
     val lhs1 = lhs.asInstanceOf[Expr]
     val rhs1 = rhs.asInstanceOf[Expr]
     
     // under assumption of having appropriate representation of equality operations and
     //		relational operations available for Ferry structural types
     if ( Set(BinOp.Feq, BinOp.Fnq) contains op ) {
        if ( lhs1.tpt != rhs1.tpt ) {
          throw new Error("ERROR: Type error when calculating a type for " + e + " expression")
        } else {
          e.tpt_=(Fa(Atom.Fbool)); return e
        }
      }
     if ( Set(BinOp.Flt, BinOp.Fle, BinOp.Fgt, BinOp.Fge) contains op ) {
        // if ( lhs1.tpt != Fa(Atom.Fint) || rhs1.tpt != Fa(Atom.Fint) ) {
        if ( lhs1.tpt != rhs1.tpt ) {
          throw new Error("ERROR: Type error when calculating a type for " + e + " expression")
        } else {
          e.tpt_=(Fa(Atom.Fbool)); return e
        }
      }
     // ---------------------------------------------------------------------------------------
     if (!OfFa.check(List(lhs1.tpt, rhs1.tpt))) 
       throw new Error("ERROR: Type error when calculating a type for " + e + " expression")
     
     if ( Set(BinOp.FAnd, BinOp.FOr) contains op) {
        if ( lhs1.tpt != Fa(Atom.Fbool) || rhs1.tpt != Fa(Atom.Fbool) ) {
          throw new Error("ERROR: Type error when calculating a type for " + e + " expression")
        } else {
          e.tpt_=(Fa(Atom.Fbool))
        }
      }
      if (Set(BinOp.Fadd, BinOp.Fminus, BinOp.Fmult, BinOp.Fdiv) contains op ) {
        if ( lhs1.tpt != Fa(Atom.Fint) || rhs1.tpt != Fa(Atom.Fint) ) {
          throw new Error("ERROR: Type error when calculating a type for " + e + " expression")
        } else {
          e.tpt_=(Fa(Atom.Fint))
        }
      }
      // see above
      /*
      if ( Set(BinOp.Feq, BinOp.Fnq) contains op ) {
        if ( lhs1.tpt != rhs1.tpt ) {
          throw new Error("ERROR: Type error when calculating a type for " + e + " expression")
        } else {
          e.tpt_=(Fa(Atom.Fbool))
        }
      }
       */
      /*
      if ( Set(BinOp.Flt, BinOp.Fle, BinOp.Fgt, BinOp.Fge) contains op ) {
        if ( lhs1.tpt != Fa(Atom.Fint) || rhs1.tpt != Fa(Atom.Fint) ) {
          throw new Error("ERROR: Type error when calculating a type for " + e + " expression")
        } else {
          e.tpt_=(Fa(Atom.Fbool))
        }
      }
      */
     e
   }
   override def visit(e: TupleExpr, exprs: List[T]) = {
     
     val exprs1 = exprs map (ex => ex.asInstanceOf[Expr])
     val tpts = exprs1 map (ex => ex.tpt)
     
     // under assumption of flattening, the following check is commented
     /*
     if (!OfFb.check(tpts)) throw new Error("ERROR: Type error when calculating a type for " + e + " expression")
     */
     // under aforementioned assumption, Ft is considered instead of Fb
     /*
     val tpts1 = tpts map (t => t.asInstanceOf[Fb])
     */
     val tpts1 = tpts map (t => t.asInstanceOf[Ft])
     val cols = exprs1 map (ex => ex match { case NomAccExpr(qualifier, selector) => Some(selector.toString); case _ => None})
     
     e.tpt_=(FTuple(cols, tpts1))
     
     e
   }
   
   override def visit(e: RecordExpr, cols: List[T], exprs: List[T]) = {
     
     val exprs1 = exprs map (ex => ex.asInstanceOf[Expr])
     val tpts = exprs1 map (ex => ex.tpt)
     
     if (!OfFb.check(tpts)) throw new Error("ERROR: Type error when calculating a type for " + e + " expression")
     
     val tpts1 = tpts map (t => t.asInstanceOf[Fb])
     
     val cols1 = cols.map(col => Some(col.asInstanceOf[TableColID].id))
     
     e.tpt_=(FTuple(cols1, tpts1))
     
     e
   }
   
   override def visit(e: NatLiteral, literal: Int) = {
     e
   }
   override def visit(e: PosAccExpr, qualifier: T, selector: T) = {
     val qualifier1 = qualifier.asInstanceOf[Expr]
     
     if (!OfFTuple.check(qualifier1.tpt)) 
       throw new Error("ERROR: Type error when calculating a type for " + e + " expression")
     selector match {
       case NatLiteral(literal) => e.tpt_=((qualifier1.tpt.asInstanceOf[FTuple].apply(literal-1))._2)
       case _ => throw new Error("ERROR: Type error when calculating a type for " + e + " expression")
     }
     e
   }
   override def visit(e: ListExpr, exprs: List[T]): T = {
     
     if (exprs.isEmpty) return e
     
     val exprs1 = exprs map (ex => ex.asInstanceOf[Expr])
     
     if (!(exprs1 filter (ex => ex.tpt != exprs1(0).tpt)).isEmpty) 
       throw new Error("ERROR: Type error when calculating a type for " + e + " expression")
     
     e.tpt_=(FList(exprs1(0).tpt))
     
     e
   }
   override def visit(e: VarIDDecl, id: String) = {
     e
   }
   override def visit(e: VarUseExpr, id: T) = {
     val id1 = id.asInstanceOf[VarIDDecl]
     id1 match {
       case VarIDDecl(ident1) =>
         for (idd <- e.env) {
        	 idd match { 
        	 	case VarIDDecl(ident2) =>
        	 	  // updated
        	 	  if (ident2 == ident1) {
        	 		  e.tpt_=(idd.tpt)
        	 	  } 
        	 	case _ => 
        	 }
         }
     } 
     
     if (e.tpt == null) 
       throw new Error("ERROR: Type error when calculating a type for " + e + " var use expression")
     
     // println("           visiting VarUseExpr with VarID = " + id1 + " is done with type evaluation: " + e.tpt)
     
     e
   }
   override def visit(e: LetBindingClause, bindings: List[Tuple2[T,T]]) = {
     val bindings1 = bindings map (bind => (bind._1.asInstanceOf[VarIDDecl], bind._2.asInstanceOf[Expr]))
     for (bind <- bindings1) {
       bind._1.tpt_=(bind._2.tpt)
     }
     e
   }
   override def visit(e: LetExpr, bindings: T, body: T) = {
     e.tpt_=(body.asInstanceOf[Expr].tpt)
     e
   }
   override def visit(e: IfExpr, cond: T, thencls: T, elsecls: T) = {
     val cond1 = cond.asInstanceOf[Expr]
     val thencls1 = thencls.asInstanceOf[Expr]
     val elsecls1 = elsecls.asInstanceOf[Expr]
     
     if (cond1.tpt != Fa(Atom.Fbool))
       throw new Error("ERROR: Type error when calculating a type for " + e + " expression, cond")
     
     if (thencls1.tpt != elsecls1.tpt) {
       throw new Error("ERROR: Type error when calculating a type for " + e + " expression, then-else")
     }
     e.tpt_=(thencls1.tpt)
     
     e
   }
   override def visit(e: ForBindingClause, bindings: List[Tuple2[T,T]]) = {
     val exprs = bindings map (bind => bind._2.asInstanceOf[Expr])
     val tpts = exprs map (ex => ex.tpt)
     if (!OfFList.check(tpts)) {
       throw new Error("ERROR: Type error when checking types for " + e)
     }
     val bindings1 = bindings map (bind => (bind._1.asInstanceOf[VarIDDecl], bind._2.asInstanceOf[Expr]))
     for (bind <- bindings1) {
       bind._1.tpt_=(bind._2.tpt.asInstanceOf[FList].elemT)
     }
     e
   }
   override def visit(e: WhereClause, expr: T) = {
     val expr1 = expr.asInstanceOf[Expr]
     if (expr1.tpt != Fa(Atom.Fbool))
       throw new Error("ERROR: Type error when checking type for " + e)
     e
   }
   override def visit(e: GroupByClause, exprs: List[T]) = {
     val exprs1 = exprs map (ex => ex.asInstanceOf[Expr])
     val tpts = exprs1 map (ex => ex.tpt)
     if (!OfFa.check(tpts))
       throw new Error("ERROR: Type error when checking types for " + e)
     
     val expr = e.env.last.expr
     if (!OfFTuple.check(expr.tpt.asInstanceOf[FList].elemT))
         throw new Error("ERROR: Type error when checking types for " + e)
     val tpt1 = expr.tpt.asInstanceOf[FList].elemT.asInstanceOf[FTuple]
     val cols1 = tpt1.cols
     val elemTs1 = tpt1.elemTs map (elemT => FList(elemT))
     expr.tpt_=(FList(FTuple(cols1, elemTs1)))
     e.env.last.tpt_=(FTuple(cols1, elemTs1))
     e
   }
   override def visit(e: OrderSpec, expr: T, modifier: OrderModifier.Value) = {
     if (!OfFa.check(expr.asInstanceOf[Expr].tpt))
     	throw new Error("ERROR: Type error when checking types for " + e)
     e
   }
   override def visit(e: OrderByClause, orderspecs: List[T]) = {
     e
   }
   override def visit(e: ForExpr, bindings: T, wherecls: Option[T], groupbycls: Option[T], groupbywherecls: Option[T], orderbycls: Option[T], expr: T) = {
     e.tpt_=(FList(expr.asInstanceOf[Expr].tpt))
     e
   }
   override def visit(e: TableID, id: String) = {
     e
   }
   override def visit(e: TableColID, id: String) = {
     e
   }
   override def visit(e: TableColSpec, id: T, ctpt: Atom.Value) = {
     e
   }
   override def visit(e: TableColSpecs, specs: List[T]) = {
     e
   }
   override def visit(e: TableKeySpec, cids: List[T]) = {
     e
   }
   override def visit(e: TableKeySpecs, specs: List[T]) = {
     e
   }
   override def visit(e: TableOrderSpec, cid: T, modifier: OrderModifier.Value) = {
     e
   }
   override def visit(e: TableOrderSpecs, specs: List[T]) = {
     e
   }
   override def visit(e: TableRefExpr, id: T, cspecs: T, kspecs: T, ospecs: Option[T]) = {
     val cspecs1 = cspecs.asInstanceOf[TableColSpecs]
     val kspecs1 = kspecs.asInstanceOf[TableKeySpecs]
     val ospecs1 = ospecs match {
       case Some(specs) => Some(specs.asInstanceOf[TableOrderSpecs])
       case None => None
     } 
     
     val cols = cspecs1.specs map (spec => Some(spec.id.id)) 
     val tpts = cspecs1.specs map (spec => Fa(spec.ctpt))
     
     e.tpt_=(FList(FTuple(cols, tpts)))
     
     e
   }
   override def visit(e: NomAccExpr, expr: T, selector: T) = {
     val expr1 = expr.asInstanceOf[Expr]
     if (!OfFTuple.check(expr1.tpt))
       throw new Error("ERROR: Type error when checking types for " + e)
     val t = expr1.tpt.asInstanceOf[FTuple].apply(selector.asInstanceOf[TableColID].id)
     // println("nom acc: " + selector.asInstanceOf[TableColID].id +", type: " + t)
     t match {
       case Some(t1) => e.tpt_=(t1._2)
       case None =>
     }
     if (e.tpt == null) throw new Error("ERROR: Type error when checking types for " + e)
     e
   }
   override def visit(e: FunID, id: FunIDs.Value) = {
     e
   }
   override def visit(e: FunListArgs, exprs: List[T]) = {
     e
   }
   override def visit(e: FunCondArgs, id: T, lexpr: T, rexpr: T) = {
     e
   }
   override def visit(e: FunAppExpr, id: T, args: T) = {
     val id1 = id.asInstanceOf[FunID]
     
     id1 match {
       case FunID(FunIDs.Fappend) => 
         if (!args.isInstanceOf[FunListArgs]) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         val args1 = args.asInstanceOf[FunListArgs]
         if (args1.exprs.size != 2) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         for (expr <- args1.exprs) {
           if (!OfFList.check(expr.tpt)) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
           if (expr.tpt != args1.exprs(0).tpt) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         }
         e.tpt_=(args1.exprs(0).tpt)
       case FunID(FunIDs.Fconcat) =>
         if (!args.isInstanceOf[FunListArgs]) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         val args1 = args.asInstanceOf[FunListArgs]
         if (args1.exprs.size != 1) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         val expr = args1.exprs(0)
         if (!OfFList.check(expr.tpt)) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         val tpt = expr.tpt.asInstanceOf[FList]
         if (!OfFList.check(tpt.elemT)) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         e.tpt_=(tpt.elemT)
       case FunID(FunIDs.Ftake) =>
         if (!args.isInstanceOf[FunListArgs]) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         val args1 = args.asInstanceOf[FunListArgs]
         if (args1.exprs.size != 2) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         if (args1.exprs(0).tpt != Fa(Atom.Fint)) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         if (!OfFList.check(args1.exprs(1).tpt)) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         e.tpt_=(args1.exprs(1).tpt)
       case FunID(FunIDs.Fdrop) =>
         if (!args.isInstanceOf[FunListArgs]) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         val args1 = args.asInstanceOf[FunListArgs]
         if (args1.exprs.size != 2) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         if (args1.exprs(0).tpt != Fa(Atom.Fint)) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         if (!OfFList.check(args1.exprs(1).tpt)) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         e.tpt_=(args1.exprs(1).tpt)
       case FunID(FunIDs.Fnth) =>
         if (!args.isInstanceOf[FunListArgs]) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         val args1 = args.asInstanceOf[FunListArgs]
         if (args1.exprs.size != 2) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         if (args1.exprs(0).tpt != Fa(Atom.Fint)) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         if (!OfFList.check(args1.exprs(1).tpt)) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         e.tpt_=(args1.exprs(1).tpt.asInstanceOf[FList].elemT)
       case FunID(FunIDs.Funordered) =>
         if (!args.isInstanceOf[FunListArgs]) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         val args1 = args.asInstanceOf[FunListArgs]
         if (args1.exprs.size != 1) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         if (!OfFList.check(args1.exprs(0).tpt)) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         e.tpt_=(args1.exprs(0).tpt)
       case FunID(FunIDs.Flength) =>
         if (!args.isInstanceOf[FunListArgs]) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         val args1 = args.asInstanceOf[FunListArgs]
         if (args1.exprs.size != 1) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         if (!OfFList.check(args1.exprs(0).tpt)) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         e.tpt_=(Fa(Atom.Fint))
       case FunID(FunIDs.Fsum) =>
         if (!args.isInstanceOf[FunListArgs]) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         val args1 = args.asInstanceOf[FunListArgs]
         if (args1.exprs.size != 1) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         if (!OfFList.check(args1.exprs(0).tpt)) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         if (args1.exprs(0).tpt.asInstanceOf[FList].elemT != Fa(Atom.Fint)) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         e.tpt_=(Fa(Atom.Fint))
       case FunID(FunIDs.Fmin) =>
         if (!args.isInstanceOf[FunListArgs]) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         val args1 = args.asInstanceOf[FunListArgs]
         if (args1.exprs.size != 1) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         if (!OfFList.check(args1.exprs(0).tpt)) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         if (args1.exprs(0).tpt.asInstanceOf[FList].elemT != Fa(Atom.Fint)) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         e.tpt_=(Fa(Atom.Fint))
       case FunID(FunIDs.Fmax) =>
         if (!args.isInstanceOf[FunListArgs]) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         val args1 = args.asInstanceOf[FunListArgs]
         if (args1.exprs.size != 1) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         if (!OfFList.check(args1.exprs(0).tpt)) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         if (args1.exprs(0).tpt.asInstanceOf[FList].elemT != Fa(Atom.Fint)) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         e.tpt_=(Fa(Atom.Fint))
         
       case FunID(FunIDs.Fthe) =>
         if (!args.isInstanceOf[FunListArgs]) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         val args1 = args.asInstanceOf[FunListArgs]
         if (args1.exprs.size != 1) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         if (!OfFList.check(args1.exprs(0).tpt)) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         e.tpt_=(args1.exprs(0).tpt.asInstanceOf[FList].elemT)
         
       case FunID(FunIDs.FgroupWith) =>
         if (!args.isInstanceOf[FunCondArgs]) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         val args1= args.asInstanceOf[FunCondArgs]
         val lexpr = args1.lexpr
         val rexpr = args1.rexpr
         if (!rexpr.tpt.isInstanceOf[FList]) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         if (!lexpr.tpt.isInstanceOf[FTuple]) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         val ltpt = lexpr.tpt.asInstanceOf[FTuple]
         val rtpt = rexpr.tpt.asInstanceOf[FList]
         for (tpt <- ltpt.elemTs) {
           if (!OfFa.check(ltpt.elemTs)) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         }
         e.tpt_=(FList(rexpr.tpt))
         
       case FunID(FunIDs.Fmap) =>
         if (!args.isInstanceOf[FunCondArgs]) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         val args1= args.asInstanceOf[FunCondArgs]
         val lexpr = args1.lexpr
         val rexpr = args1.rexpr
         if (!OfFList.check(rexpr.tpt)) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         
         e.tpt_=(FList(lexpr.tpt))
         
       case FunID(FunIDs.Ffilter) =>
         if (!args.isInstanceOf[FunCondArgs]) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         val args1= args.asInstanceOf[FunCondArgs]
         val lexpr = args1.lexpr
         val rexpr = args1.rexpr
         if (!OfFList.check(rexpr.tpt)) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         if (lexpr.tpt != Fa(Atom.Fbool)) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         
         e.tpt_=(rexpr.tpt)
         
       case FunID(FunIDs.Fzip) =>
         if (!args.isInstanceOf[FunListArgs]) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         val args1 = args.asInstanceOf[FunListArgs]
         // updated with respect to 'flattening'
         // var elemTs1: List[Fb] = List()
         var elemTs1: List[Ft] = List()
         var cols1: List[Option[String]] = List()
         for (arg <- args1.exprs) {
           if (!arg.tpt.isInstanceOf[FList]) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
           val t = arg.tpt.asInstanceOf[FList].elemT
           // updated with respect to 'flattening'
           // if (!t.isInstanceOf[Fb]) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
           // elemTs1 = elemTs1:::List(t.asInstanceOf[Fb])
           elemTs1 = elemTs1:::List(t.asInstanceOf[Ft])                                            
           if (arg.isInstanceOf[NomAccExpr]) {
             if (!cols1.contains(Some(arg.asInstanceOf[NomAccExpr].selector.id))) {
            	 cols1 = cols1:::List(Some(arg.asInstanceOf[NomAccExpr].selector.id))
             } else {
               // TODO: adding column fresh name instead of None if duplicated
               cols1 = cols1:::List(None)
             }
           } else {
             cols1 = cols1:::List(None)
           }
         } 
         // TODO: checking for NomAccExpr for more deep column name propagation to the outer tuple
         
         e.tpt_=(FList(FTuple(cols1, elemTs1)))
         
       case FunID(FunIDs.Funzip) =>
         if (!args.isInstanceOf[FunListArgs]) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         val args1 = args.asInstanceOf[FunListArgs]
         if (args1.exprs.size != 1) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         val arg = args1.exprs(0)
         if (!arg.tpt.isInstanceOf[FList]) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         val elemT = arg.tpt.asInstanceOf[FList].elemT
         if (!elemT.isInstanceOf[FTuple]) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         val cols1 = elemT.asInstanceOf[FTuple].cols
         val elemTs = elemT.asInstanceOf[FTuple].elemTs
         var elemTs1: List[FList] = elemTs map (elem => FList(elem))
         
         e.tpt_=(FTuple(cols1, elemTs1))
         
       case FunID(FunIDs.FsortBy) =>
         if (!args.isInstanceOf[FunCondArgs]) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         val args1= args.asInstanceOf[FunCondArgs]
         val lexpr = args1.lexpr
         val rexpr = args1.rexpr
         if (!rexpr.tpt.isInstanceOf[FList]) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         if (!lexpr.tpt.isInstanceOf[FTuple]) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         val ltpt = lexpr.tpt.asInstanceOf[FTuple]
         val rtpt = rexpr.tpt.asInstanceOf[FList]
         for (tpt <- ltpt.elemTs) {
           if (!tpt.isInstanceOf[Fa]) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         }
         e.tpt_=(FList(rexpr.tpt))
         
       case FunID(FunIDs.FgroupBy) =>
         if (!args.isInstanceOf[FunCondArgs]) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature 1")
         val args1= args.asInstanceOf[FunCondArgs]
         val lexpr = args1.lexpr
         val rexpr = args1.rexpr
         if (!rexpr.tpt.isInstanceOf[FList]) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature 2")
         if (!lexpr.tpt.isInstanceOf[FTuple]) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature 3")
         val ltpt = lexpr.tpt.asInstanceOf[FTuple]
         val rtpt = rexpr.tpt.asInstanceOf[FList]
         if (!rtpt.elemT.isInstanceOf[FTuple]) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature 4")
         val cols1 = rtpt.elemT.asInstanceOf[FTuple].cols
         val elemTs = rtpt.elemT.asInstanceOf[FTuple].elemTs
         val elemTs1 = elemTs map (elemT => FList(elemT))
         for (tpt <- ltpt.elemTs) {
           if (!tpt.isInstanceOf[Fa]) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature 5")
         }
         e.tpt_=(FList(FTuple(cols1, elemTs1)))
       
       // updated: custom operator
       case FunID(FunIDs.Frange) =>
         if (!args.isInstanceOf[FunListArgs]) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         val args1 = args.asInstanceOf[FunListArgs]
         if (args1.exprs.size != 2) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         if ((args1.exprs(0).tpt != Fa(Atom.Fint)) | (args1.exprs(1).tpt != Fa(Atom.Fint))) throw new Error("ERROR: Type error when checking types for " + e + ": wrong function signature")
         e.tpt_=(FList(Fa(Atom.Fint)))
         
     }
     e
   }
   override def visit(e: ParenthesizedExpr, expr: T) = {
     e
   }
   
   abstract class TypeChecker {
     
     def check(tpt: Ft): Boolean 
   
     def check(tpts: List[Ft]): Boolean
   }
   
   object OfFb extends TypeChecker { 
     type A = Fb
     override def check(tpt: Ft): Boolean = {
    		 if (tpt.isInstanceOf[A]) {
    			 true
    		 } else {
    			 false
    		 }
     }
     override def check(tpts: List[Ft]): Boolean = {
    		 if ((tpts filter (tpt => tpt.isInstanceOf[A])).size == tpts.size) {
    			 true
    		 } else {
    			 false
    		 }
     }
   }
   object OfFTuple extends TypeChecker { 
     type A = FTuple 
     override def check(tpt: Ft): Boolean = {
    		 if (tpt.isInstanceOf[A]) {
    			 true
    		 } else {
    			 false
    		 }
     }
   
     override def check(tpts: List[Ft]): Boolean = {
    		 if ((tpts filter (tpt => tpt.isInstanceOf[A])).size == tpts.size) {
    			 true
    		 } else {
    			 false
    		 }
     }
   }
   object OfFList extends TypeChecker { 
     type A = FList 
     override def check(tpt: Ft): Boolean = {
    		 if (tpt.isInstanceOf[A]) {
    			 true
    		 } else {
    			 false
    		 }
     }
   
     override def check(tpts: List[Ft]): Boolean = {
    		 if ((tpts filter (tpt => tpt.isInstanceOf[A])).size == tpts.size) {
    			 true
    		 } else {
    			 false
    		 }
     }
   }
   object OfFa extends TypeChecker { 
     type A = Fa 
     override def check(tpt: Ft): Boolean = {
    		 if (tpt.isInstanceOf[A]) {
    			 true
    		 } else {
    			 false
    		 }
     }
   
     override def check(tpts: List[Ft]): Boolean = {
    		 if ((tpts filter (tpt => tpt.isInstanceOf[A])).size == tpts.size) {
    			 true
    		 } else {
    			 false
    		 }
     }
   }
  
}

object Typer extends sts.ferry.AbstractSyntax.Walker(TyperVisitor, sts.ferry.AbstractSyntax.StringLiteral("unknown node")) {
  
  import AbstractSyntax._
  
  def typing(e: Expr): FerryAttr = {
    this walk e
  } 

}
