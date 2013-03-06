package edu.depauw.relalg

trait Aggregation {
  def apply(schema: Schema)(group: Iterable[Row]): Value
  
  def typeGiven(schema: Schema): Type
}

case class MinAggregation(name: String) extends Aggregation {
  def apply(schema: Schema)(group: Iterable[Row]): Value =
    group.map(schema(name)(_)).min
    
  def typeGiven(schema: Schema): Type = schema.typeOf(name)
}

case class MaxAggregation(name: String) extends Aggregation {
  def apply(schema: Schema)(group: Iterable[Row]): Value =
    group.map(schema(name)(_)).max
    
  def typeGiven(schema: Schema): Type = schema.typeOf(name)
}

case class SumAggregation(name: String) extends Aggregation {
  def apply(schema: Schema)(group: Iterable[Row]): Value =
    group.map(schema(name)(_).asInt).sum
    
  def typeGiven(schema: Schema): Type = IntType
}

case class AvgAggregation(name: String) extends Aggregation {
  def apply(schema: Schema)(group: Iterable[Row]): Value =
    group.map(schema(name)(_).asInt).sum / group.size
    
  def typeGiven(schema: Schema): Type = IntType
}

case object CountAggregation extends Aggregation {
  def apply(schema: Schema)(group: Iterable[Row]): Value =
    group.size
    
  def typeGiven(schema: Schema): Type = IntType
}