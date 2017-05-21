package clunk

import clunk.Ast.Node._
import clunk.Ast.Node.Comparator._
import clunk.TypeTag._

class SqlBuilder(table: String, select: SelectNode, where: Option[WhereNode]) {

  def toSql() = {
    (buildSelect _).
      andThen(buildFrom).
      andThen(buildWhere)(new StringBuilder).
      toString
  }

  private def buildSelect(builder: StringBuilder) =
    select.columns.map({ c => s"`${c.srcName}`" }).
      addString(builder, "SELECT ", ", ", "")

  private def buildFrom(builder: StringBuilder) =
    builder.append(s" FROM `${table}`")

  private def buildWhere(builder: StringBuilder) = {
    def buildComparisons(comparisons: Seq[Comparison[_]]) = {
      comparisons.map({ c =>
        c.comparator match {
          case EqualTo => buildComparison("=", c.column, c.value)
          case LessThan => buildComparison("<", c.column, c.value)
          case MoreThan => buildComparison(">", c.column, c.value)
        }
      }).addString(builder, " WHERE ", " AND ", "")
    }

    where.map(_.comparisons).map(buildComparisons(_)).getOrElse(builder)
  }

  private def buildComparison(op: String, c: Column[_], v: Any) = {
    val value = c.typeTag match {
      case StrTag => s"'${v}'"
      case _      => s"${v}"
    }

    s"`${c.srcName}` ${op} ${value}"
  }
}
