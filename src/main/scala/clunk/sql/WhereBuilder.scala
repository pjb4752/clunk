package clunk.sql

import clunk.Column
import clunk.Ast.Node._
import clunk.Ast.Node.Comparator._
import clunk.TypeTag._

class WhereBuilder(where: Option[WhereNode]) {

  def toSql(builder: StringBuilder): StringBuilder =
    where.map({ w => buildFilters(builder, w.tableFilters) }).
      getOrElse(builder)

  private def buildFilters(builder: StringBuilder,
      filters: Seq[TableWhereNode]) = {

    if (!filters.isEmpty) builder.append(" WHERE ")
    buildComparisons(builder, filters.flatMap(_.comparisons))
  }

  private def buildComparisons(builder: StringBuilder,
      comparisons: Seq[Comparison[_, _]]) = {

    comparisons.map({ c =>
      val cmpBuilder = (buildComparison _)(c.column, c.value)

      c.comparator match {
        case EqualTo => cmpBuilder("=")
        case LessThan => cmpBuilder("<")
        case MoreThan => cmpBuilder(">")
      }
    }).addString(builder, " AND ")
  }

  private def buildComparison(c: Column[_, _], v: Any)(op: String) = {
    val value = c.typeTag match {
      case StrTag => s"'${v}'"
      case _      => s"${v}"
    }

    s"`${c.table.srcName}`.`${c.srcName}` ${op} ${value}"
  }
}
