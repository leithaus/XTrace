package kiama.example.picojava.benchmark

import kiama.attribution._
import kiama.example.picojava.AbstractSyntax._
import kiama.example.picojava.ErrorCheck._

object PicoJavaBenchmark extends Application {
    // For the actual program text this is based on, see DotNameResolutionTests.pj

    def basicAst = ClassDecl ("AA", None, Block (List (VarDecl ("x", Use ("int")))))

    def createAst (subtree : ClassDecl) =
        ClassDecl ("AA", None, Block (
              List (VarDecl ("y", Use ("int")),
                    VarDecl ("a", Use ("AA")),
                    AssignStmt (Use ("x"),
                                Dot (Use ("a"), Use ("x"))),
                    subtree,
                    ClassDecl ("BB", Some (Use ("AA")), Block (
                        List (VarDecl ("b", Use ("BB")),
                              AssignStmt (Dot (Use ("b"), Use("y")),
                                          Dot (Use ("b"), Use("x")))))))))

    def createProgram (subtree : ClassDecl) =
        Program (Block (List (subtree)))

    // Warm up the JIT compiler

    for (i <- 0 until 15000) {
        val result = createProgram(createAst(createAst(basicAst)))->errors
        Attribution.resetMemo
        DynamicAttribution.resetMemo
    }

    System.gc

    // Two-step benchmark

    // Initialize inputs

    val inputs = new scala.collection.mutable.ArrayBuffer[Program]
    for (i <- 0 until 100) {
        var bigAsst = createAst(basicAst)
        for (i <- 0 until 150) bigAsst = createAst(bigAsst)
        inputs += createProgram(bigAsst)
    }

    // Evaluate some attributes

    var result = 0
    val start = System.currentTimeMillis

    for (i <- 0 until inputs.size) {
        val p = inputs(i)
        inputs(i) = null
        result = (p->errors).size
        Attribution.resetMemo
        DynamicAttribution.resetMemo
    }

    println((System.currentTimeMillis - start))

  /*
    var time : Long = 0
    var result : Int = 0

    for (i <- 0 until 100) {
        var bigAsst = createAst(basicAst)
        for (j <- 0 until 150) bigAsst = createAst(bigAsst)
        val p = createProgram(bigAsst)

        val start = System.nanoTime
        result = (p->errors).size
        time += (System.nanoTime - start)

        Attribution.resetMemo
        DynamicAttribution.resetMemo
    }

    println((time / 1000000))
    */
}
