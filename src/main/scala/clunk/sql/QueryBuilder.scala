package clunk.sql

import clunk.Ast.Node._

class QueryBuilder(select: SelectNode, where: Option[WhereNode]) {
  val selectBuilder = new SelectBuilder(select)
  val whereBuilder = new WhereBuilder(where)

  def toSql() =
    (buildSelect _).
      andThen(buildFrom).
      andThen(buildWhere)(new StringBuilder).
      toString

  private def buildSelect(builder: StringBuilder) =
    selectBuilder.toSql(builder)

  private def buildFrom(builder: StringBuilder) =
    builder.append(s" FROM `${select.tableSelects.head.table.srcName}`")

  private def buildWhere(builder: StringBuilder) =
    whereBuilder.toSql(builder)
}
