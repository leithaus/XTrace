package com.biosimilarity.validation.model.trace;

import com.biosimilarity.validation.model.trace.Absyn.*;
import java.util.Collections;
import java.util.List;
import java.util.ArrayList;

/** BNFC-Generated Fold Visitor */
public abstract class FoldVisitor<R,A> implements AllVisitor<R,A> {
    public abstract R leaf(A arg);
    public abstract R combine(R x, R y, A arg);

/* Expression */
    public R visit(com.biosimilarity.validation.model.trace.Absyn.Sequence p, A arg) {
      R r = leaf(arg);
      r = combine(p.expression_1.accept(this, arg), r, arg);
      r = combine(p.expression_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.validation.model.trace.Absyn.Application p, A arg) {
      R r = leaf(arg);
      r = combine(p.expression_1.accept(this, arg), r, arg);
      r = combine(p.expression_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.validation.model.trace.Absyn.Mention p, A arg) {
      R r = leaf(arg);
      r = combine(p.variableexpr_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.validation.model.trace.Absyn.Value p, A arg) {
      R r = leaf(arg);
      r = combine(p.valueexpr_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.validation.model.trace.Absyn.Abstraction p, A arg) {
      R r = leaf(arg);
      for (VariableExpr x : p.listvariableexpr_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      r = combine(p.expression_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.validation.model.trace.Absyn.Stipulation p, A arg) {
      R r = leaf(arg);
      r = combine(p.variableexpr_.accept(this, arg), r, arg);
      r = combine(p.expression_.accept(this, arg), r, arg);
      return r;
    }

/* VariableExpr */
    public R visit(com.biosimilarity.validation.model.trace.Absyn.Transcription p, A arg) {
      R r = leaf(arg);
      r = combine(p.expression_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.validation.model.trace.Absyn.AtomLiteral p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* ValueExpr */
    public R visit(com.biosimilarity.validation.model.trace.Absyn.Numeric p, A arg) {
      R r = leaf(arg);
      return r;
    }


}
