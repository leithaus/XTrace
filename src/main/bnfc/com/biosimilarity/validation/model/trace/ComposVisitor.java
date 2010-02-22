package com.biosimilarity.validation.model.trace;
import com.biosimilarity.validation.model.trace.Absyn.*;
/** BNFC-Generated Composition Visitor
*/

public class ComposVisitor<A> implements
  com.biosimilarity.validation.model.trace.Absyn.Expression.Visitor<com.biosimilarity.validation.model.trace.Absyn.Expression,A>,
  com.biosimilarity.validation.model.trace.Absyn.VariableExpr.Visitor<com.biosimilarity.validation.model.trace.Absyn.VariableExpr,A>,
  com.biosimilarity.validation.model.trace.Absyn.ValueExpr.Visitor<com.biosimilarity.validation.model.trace.Absyn.ValueExpr,A>
{
/* Expression */
    public Expression visit(com.biosimilarity.validation.model.trace.Absyn.Sequence p, A arg)
    {
      Expression expression_1 = p.expression_1.accept(this, arg);
      Expression expression_2 = p.expression_2.accept(this, arg);

      return new com.biosimilarity.validation.model.trace.Absyn.Sequence(expression_1, expression_2);
    }
    public Expression visit(com.biosimilarity.validation.model.trace.Absyn.Application p, A arg)
    {
      Expression expression_1 = p.expression_1.accept(this, arg);
      Expression expression_2 = p.expression_2.accept(this, arg);

      return new com.biosimilarity.validation.model.trace.Absyn.Application(expression_1, expression_2);
    }
    public Expression visit(com.biosimilarity.validation.model.trace.Absyn.Mention p, A arg)
    {
      VariableExpr variableexpr_ = p.variableexpr_.accept(this, arg);

      return new com.biosimilarity.validation.model.trace.Absyn.Mention(variableexpr_);
    }
    public Expression visit(com.biosimilarity.validation.model.trace.Absyn.Value p, A arg)
    {
      ValueExpr valueexpr_ = p.valueexpr_.accept(this, arg);

      return new com.biosimilarity.validation.model.trace.Absyn.Value(valueexpr_);
    }
    public Expression visit(com.biosimilarity.validation.model.trace.Absyn.Abstraction p, A arg)
    {
      ListVariableExpr listvariableexpr_ = new ListVariableExpr();
      for (VariableExpr x : p.listvariableexpr_) {
        listvariableexpr_.add(x.accept(this,arg));
      }
      Expression expression_ = p.expression_.accept(this, arg);

      return new com.biosimilarity.validation.model.trace.Absyn.Abstraction(listvariableexpr_, expression_);
    }
    public Expression visit(com.biosimilarity.validation.model.trace.Absyn.Stipulation p, A arg)
    {
      VariableExpr variableexpr_ = p.variableexpr_.accept(this, arg);
      Expression expression_ = p.expression_.accept(this, arg);

      return new com.biosimilarity.validation.model.trace.Absyn.Stipulation(variableexpr_, expression_);
    }

/* VariableExpr */
    public VariableExpr visit(com.biosimilarity.validation.model.trace.Absyn.Transcription p, A arg)
    {
      Expression expression_ = p.expression_.accept(this, arg);

      return new com.biosimilarity.validation.model.trace.Absyn.Transcription(expression_);
    }
    public VariableExpr visit(com.biosimilarity.validation.model.trace.Absyn.AtomLiteral p, A arg)
    {
      String ident_ = p.ident_;

      return new com.biosimilarity.validation.model.trace.Absyn.AtomLiteral(ident_);
    }

/* ValueExpr */
    public ValueExpr visit(com.biosimilarity.validation.model.trace.Absyn.Numeric p, A arg)
    {
      Integer integer_ = p.integer_;

      return new com.biosimilarity.validation.model.trace.Absyn.Numeric(integer_);
    }

}