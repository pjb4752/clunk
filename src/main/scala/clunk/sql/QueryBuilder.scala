package clunk.sql

import clunk.Ast.Node._

class QueryBuilder(
  select: SelectNode,
  join: Option[JoinNode],
  where: Option[WhereNode]) {

  val selectBuilder = new SelectBuilder(select, join)
  val joinBuilder = new JoinBuilder(join)
  val whereBuilder = new WhereBuilder(where)

  def toSql() =
    (buildSelect _).
      andThen(buildFrom).
      andThen(buildJoin).
      andThen(buildWhere)(new StringBuilder).
      toString

  private def buildSelect(builder: StringBuilder) =
    selectBuilder.toSql(builder)

  private def buildFrom(builder: StringBuilder) =
    builder.append(s" FROM `${select.tableSelects.head.table.srcName}`")

  private def buildJoin(builder: StringBuilder) =
    joinBuilder.toSql(builder)

  private def buildWhere(builder: StringBuilder) =
    whereBuilder.toSql(builder)
}
