package clunk.queries

import clunk.Ast.Node._
import clunk.jdbc.Database
import clunk.Mapping
import clunk.sql.QueryBuilder
import clunk.Table

class Query5[T1 <: Table, T2 <: Table, T3 <: Table, T4 <: Table, T5 <: Table](
  val source: Tuple5[T1, T2, T3, T4, T5],
  val select: SelectNode,
  val join: Option[JoinNode],
  val where: Option[WhereNode]) {

  def filter(f: Tuple5[T1, T2, T3, T4, T5] => Comparison[_]) = {
    val newWhere = Builder.buildWhere(where, f(source))
    new Query5(source, select, join, newWhere)
  }

  def toSql() = new QueryBuilder(select, join, where).toSql

  def result = {
    val sql = new QueryBuilder(select, join, where).toSql
    val rs = Database.connection.execute(sql)

    Mapping.map5(source, rs)
  }
}
