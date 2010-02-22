package com.biosimilarity.validation.model.trace;
import com.biosimilarity.validation.model.trace.Absyn.*;
/** BNFC-Generated Abstract Visitor */
public class AbstractVisitor<R,A> implements AllVisitor<R,A> {
/* Expression */
    public R visit(com.biosimilarity.validation.model.trace.Absyn.Sequence p, A arg) { return visitDefault(p, arg); }

    public R visit(com.biosimilarity.validation.model.trace.Absyn.Application p, A arg) { return visitDefault(p, arg); }

    public R visit(com.biosimilarity.validation.model.trace.Absyn.Mention p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.validation.model.trace.Absyn.Value p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.validation.model.trace.Absyn.Abstraction p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.validation.model.trace.Absyn.Stipulation p, A arg) { return visitDefault(p, arg); }

    public R visitDefault(com.biosimilarity.validation.model.trace.Absyn.Expression p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* VariableExpr */
    public R visit(com.biosimilarity.validation.model.trace.Absyn.Transcription p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.validation.model.trace.Absyn.AtomLiteral p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.validation.model.trace.Absyn.VariableExpr p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* ValueExpr */
    public R visit(com.biosimilarity.validation.model.trace.Absyn.Numeric p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.validation.model.trace.Absyn.ValueExpr p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }

}
