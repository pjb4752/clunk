package clunk.queries

import clunk.Ast.Node._
import clunk.jdbc.Database
import clunk.mappers.ResultSetMapper3
import clunk.sql.QueryBuilder
import clunk.Table

class Query3[A <: Table[_], B <: Table[_], C <: Table[_]](
  val source: Tuple3[A, B, C],
  val select: SelectNode,
  val join: Option[JoinNode],
  val where: Option[WhereNode]) {

  def filter(f: Tuple3[A, B, C] => Comparison[_]) = {
    val newWhere = Builder.buildWhere(where, f(source))
    new Query3(source, select, join, newWhere)
  }

  def toSql() = new QueryBuilder(select, join, where).toSql

  def result = {
    val sql = new QueryBuilder(select, join, where).toSql
    val rs = Database.connection.execute(sql)

    new ResultSetMapper3[A, B, C](source).map(rs)
  }
}
