package clunk.sql

import clunk.Column
import clunk.Ast.Node._
import clunk.Ast.Node.Comparator._
import clunk.TypeTag._

class WhereBuilder(where: Option[WhereNode]) {

  def toSql(builder: StringBuilder): StringBuilder =
    where.map({ w => buildFilters(builder, w.tableFilters) }).
      getOrElse(builder)

  def toParams = where.map({ w =>
      w.tableFilters.flatMap(_.comparisons)
    }).getOrElse(Seq[Comparison[_, _]]())

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
    s"`${c.table.srcName}`.`${c.srcName}` ${op} ?"
  }
}
