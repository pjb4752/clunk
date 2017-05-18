package clunk

import clunk.Ast.Node._
import clunk.Ast.Node.Comparator._

class Column[A](val typeTag: TypeTag, val srcName: String)
  extends ColumnLike[A] {

  def isEqualTo(value: A) = makeComparison(EqualTo, value)
  def isLessThan(value: A) = makeComparison(LessThan, value)
  def isMoreThan(value: A) = makeComparison(MoreThan, value)

  private def makeComparison(comparator: Comparator, value: A) =
    new Comparison[A](comparator, this, value)
}
