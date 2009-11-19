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

object FerryTest {
  
  import AbstractSyntax._
  
  def main(args:Array[String]) {
    
    import Typer._
    
    var e: Expr = null
    
    // not true
    // e = UnOpExpr(BoolLiteral(true),UnOp.FNot)
    
    // 1 + 2
    // e = BinOpExpr(IntLiteral(1), IntLiteral(2), BinOp.Fadd)
    
    // true and true
    // e = BinOpExpr(BoolLiteral(true), BoolLiteral(true), BinOp.FAnd)
    
    // 1 == 2
    // e = BinOpExpr(IntLiteral(1), IntLiteral(2), BinOp.Feq)
    
    // 1 < 2
    // e = BinOpExpr(IntLiteral(1), IntLiteral(2), BinOp.Flt)
    
    // 1 < 1 + 2
    // e = BinOpExpr(IntLiteral(1), BinOpExpr(IntLiteral(1), IntLiteral(2), BinOp.Fadd), BinOp.Flt)
    
    // (1 < 1 + 2, 1 + 2).2
    // e = PosAccExpr(TupleExpr(List(BinOpExpr(IntLiteral(1), BinOpExpr(IntLiteral(1), IntLiteral(2), BinOp.Fadd), BinOp.Flt), BinOpExpr(IntLiteral(1), IntLiteral(2), BinOp.Fadd))),NatLiteral(2))
    
    // [(1 < 1 + 2, 1 + 2).2, 55]
    // e = ListExpr(List(PosAccExpr(TupleExpr(List(BinOpExpr(IntLiteral(1), BinOpExpr(IntLiteral(1), IntLiteral(2), BinOp.Fadd), BinOp.Flt), BinOpExpr(IntLiteral(1), IntLiteral(2), BinOp.Fadd))),NatLiteral(2)), IntLiteral(55)))
    
    // [1,true] - not well-formed
    // e = ListExpr(List(IntLiteral(1), BoolLiteral(true)))
    
    // let v = 1 in v + 2
    // e = LetExpr(LetBindingClause(List((VarIDDecl("v"),IntLiteral(1)))),BinOpExpr(VarUseExpr(VarIDDecl("v")),IntLiteral(2),BinOp.Fadd))
    
    // if 1 < 1 + 2 then 1 else let v = 1 in v + 2
    // e = IfExpr(BinOpExpr(IntLiteral(1), BinOpExpr(IntLiteral(1), IntLiteral(2), BinOp.Fadd), BinOp.Flt), IntLiteral(1), LetExpr(LetBindingClause(List((VarIDDecl("v"),IntLiteral(1)))),BinOpExpr(VarUseExpr(VarIDDecl("v")),IntLiteral(2),BinOp.Fadd)))
    
    // for v in [(1, 2),(3, 4)]  group by 1 return v.1
    // e = ForExpr(ForBindingClause(List((VarIDDecl("v"), ListExpr(List(TupleExpr(List(IntLiteral(1),IntLiteral(2))),TupleExpr(List(IntLiteral(3),IntLiteral(4)))))))), None, Some(GroupByClause(List(IntLiteral(1)))),None,None,PosAccExpr(VarUseExpr(VarIDDecl("v")),NatLiteral(1)))
    
    // let e =  table Employees (id int, dept string, salary int) with keys ((id)) 
    //		in for x in e  group by x.dept 
    //			return (the(x.dept), take(2, for y in zip(x.id, x.salary) return y))
    e = LetExpr(LetBindingClause(List((VarIDDecl("e"),TableRefExpr(TableID("Employees"), TableColSpecs(List(TableColSpec(TableColID("id"), Atom.Fint), TableColSpec(TableColID("dept"), Atom.Fstring), TableColSpec(TableColID("salary"), Atom.Fint))), TableKeySpecs(List(TableKeySpec(List(TableColID("id"))))), None ) ))), ForExpr(ForBindingClause(List( (VarIDDecl("x"), VarUseExpr(VarIDDecl("e"))) )), None, Some(GroupByClause( List(NomAccExpr(VarUseExpr(VarIDDecl("x")), TableColID("dept"))))), None, None, TupleExpr(List(FunAppExpr(FunID(FunIDs.Fthe),FunListArgs(List(NomAccExpr(VarUseExpr(VarIDDecl("x")), TableColID("dept"))))), FunAppExpr( FunID(FunIDs.Ftake), FunListArgs ( List( IntLiteral(2), ForExpr( ForBindingClause( List(Tuple2(VarIDDecl("y"), FunAppExpr( FunID(FunIDs.Fzip), FunListArgs(List(NomAccExpr(VarUseExpr(VarIDDecl("x")), TableColID("id")), NomAccExpr(VarUseExpr(VarIDDecl("x")), TableColID("salary"))) ) ) ) ) ), None, None, None, Some(OrderByClause(List(OrderSpec(PosAccExpr(VarUseExpr(VarIDDecl("y")), NatLiteral(2)), OrderModifier.Fdescending)))), VarUseExpr(VarIDDecl("y"))) ) ) )))))
    
    // concat([[1],[1]])
    // e = FunAppExpr(FunID(FunIDs.Fconcat), FunListArgs(List(ListExpr(List(ListExpr(List(IntLiteral(1))),ListExpr(List(IntLiteral(1))))))))
    
    // TODO: all functions have to be tested
    
    // unzip([(1, str1, true),(2, str2, false)])
    // e = FunAppExpr( FunID(FunIDs.Funzip), FunListArgs(List(ListExpr(List(TupleExpr(List(IntLiteral(1),StringLiteral("str1"),BoolLiteral(true))),TupleExpr(List(IntLiteral(2),StringLiteral("str2"),BoolLiteral(false))) )))) )
    
    // groupBy(v -> (true, true), [(1, str1, true),(2, str2, false)])
    // e = FunAppExpr( FunID(FunIDs.FgroupBy), FunCondArgs(VarIDDecl("v"), TupleExpr(List(BoolLiteral(true),BoolLiteral(true))), ListExpr(List(TupleExpr(List(IntLiteral(1),StringLiteral("str1"),BoolLiteral(true))),TupleExpr(List(IntLiteral(2),StringLiteral("str2"),BoolLiteral(false))) ))) )
    
    // groupWith(v -> (true, true), [(1, str1, true),(2, str2, false)])
    // e = FunAppExpr( FunID(FunIDs.FgroupWith), FunCondArgs(VarIDDecl("v"), TupleExpr(List(BoolLiteral(true),BoolLiteral(true))), ListExpr(List(TupleExpr(List(IntLiteral(1),StringLiteral("str1"),BoolLiteral(true))),TupleExpr(List(IntLiteral(2),StringLiteral("str2"),BoolLiteral(false))) ))) )
    
    // sortBy(v -> (true, true), [(1, str1, true),(2, str2, false)])
    // e = FunAppExpr( FunID(FunIDs.FsortBy), FunCondArgs(VarIDDecl("v"), TupleExpr(List(BoolLiteral(true),BoolLiteral(true))), ListExpr(List(TupleExpr(List(IntLiteral(1),StringLiteral("str1"),BoolLiteral(true))),TupleExpr(List(IntLiteral(2),StringLiteral("str2"),BoolLiteral(false))) ))) )
    
    // map(v -> (true, true), [(1, str1, true),(2, str2, false)])
    // e = FunAppExpr( FunID(FunIDs.Fmap), FunCondArgs(VarIDDecl("v"), TupleExpr(List(BoolLiteral(true),BoolLiteral(true))), ListExpr(List(TupleExpr(List(IntLiteral(1),StringLiteral("str1"),BoolLiteral(true))),TupleExpr(List(IntLiteral(2),StringLiteral("str2"),BoolLiteral(false))) ))) )
    
    // filter(v -> true, [(1, str1, true),(2, str2, false)])
    // e = FunAppExpr( FunID(FunIDs.Ffilter), FunCondArgs(VarIDDecl("v"), BoolLiteral(true), ListExpr(List(TupleExpr(List(IntLiteral(1),StringLiteral("str1"),BoolLiteral(true))),TupleExpr(List(IntLiteral(2),StringLiteral("str2"),BoolLiteral(false))) ))) )
    
    // (name John, dept UK, salary 1)
    e = RecordExpr(List(TableColID("name"), TableColID("dept"), TableColID("salary")), List(StringLiteral("John"), StringLiteral("UK"), IntLiteral(1)))
    
    // [(name John, dept UK, salary 200),(name George, dept US, salary 400)]
    // 		with type: [('name' string, 'dept' string, 'salary' int)]
    e = ListExpr(List(RecordExpr(List(TableColID("name"), TableColID("dept"), TableColID("salary")), List(StringLiteral("John"), StringLiteral("UK"), IntLiteral(200))), RecordExpr(List(TableColID("name"), TableColID("dept"), TableColID("salary")), List(StringLiteral("George"), StringLiteral("US"), IntLiteral(400)))))
    
    // map(x -> let y1 = x.1, y2 = x.2 in y1, [(1, 2),(3, 4)])
    e = FunAppExpr(FunID(FunIDs.Fmap), FunCondArgs(VarIDDecl("x"), LetExpr(LetBindingClause(List((VarIDDecl("y1"), PosAccExpr(VarUseExpr(VarIDDecl("x")), NatLiteral(1))),(VarIDDecl("y2"), PosAccExpr(VarUseExpr(VarIDDecl("x")), NatLiteral(2))))), VarUseExpr(VarIDDecl("y1"))), ListExpr(List(TupleExpr(List(IntLiteral(1),IntLiteral(2))), TupleExpr(List(IntLiteral(3),IntLiteral(4)))))))
    println("Initial expr: " + e.toString)
    
    typing(e) 
    
    print("Typed expr: " + e)
    if (e.isInstanceOf[Expr]) println(" with type: " + e.asInstanceOf[Expr].tpt)
    
  }

}
