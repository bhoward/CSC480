package edu.depauw.relalg

trait Condition {
  def apply(schema: Schema)(row: Row): Boolean
}

case class OpCondition(expr1: Expression, expr2: Expression, op: Int => Boolean) extends Condition {
  def apply(schema: Schema)(row: Row): Boolean =
    op(expr1(schema)(row) compareTo expr2(schema)(row))
}