package clunk

import clunk.Ast.Node._
import clunk.Ast.Node.Comparator._

class Column[A, B](val typeTag: TypeTag, val table: Table,
    val srcName: String, val isNullable: Boolean = false)
  extends ColumnLike[A] {

  def isEqualTo(value: B) = makeComparison(EqualTo, value)
  def isLessThan(value: B) = makeComparison(LessThan, value)
  def isMoreThan(value: B) = makeComparison(MoreThan, value)

  override def toString() =
    List(srcName, dbType, nullValue).mkString(" ")

  private def makeComparison(comparator: Comparator, value: B) =
    new Comparison[A, B](comparator, this, value)

  private def dbType = typeTag match {
    case TypeTag.IntTag => "INT"
    case TypeTag.StrTag => "VARCHAR(255)"
  }

  private def nullValue = if (isNullable) "" else "NOT NULL"
}
