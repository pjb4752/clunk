package clunk.queries

import clunk.Ast.Node._
import clunk.jdbc.Database
import clunk.Mapping
import clunk.sql.QueryBuilder
import clunk.Table

class Query3[T1 <: Table, T2 <: Table, T3 <: Table](
  val source: Tuple3[T1, T2, T3],
  val select: SelectNode,
  val join: Option[JoinNode],
  val where: Option[WhereNode]) {

  def filter(f: Tuple3[T1, T2, T3] => Comparison[_]) = {
    val newWhere = Builder.buildWhere(where, f(source))
    new Query3(source, select, join, newWhere)
  }

  def toSql() = new QueryBuilder(select, join, where).toSql

  def result = {
    val sql = new QueryBuilder(select, join, where).toSql
    val rs = Database.connection.execute(sql)

    Mapping.map3(source, rs)
  }
}
