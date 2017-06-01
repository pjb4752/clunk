package clunk

import clunk.Ast.Node._
import clunk.Ast.Node.Comparator._
import clunk.jdbc.Database
import clunk.mappers.ResultSetMapper
import clunk.queries.Query2
import clunk.sql.QueryBuilder

class Query[T1 <: Table](
  val source: T1,
  val select: SelectNode,
  val join: Option[JoinNode],
  val where: Option[WhereNode]) {

  def filter(f: T1 => Comparison[_]) = {
    val newWhere = queries.Builder.buildWhere(where, f(source))
    new Query(source, select, join, newWhere)
  }

  def innerJoin[T2 <: Table, A](f: T1 => Association[T1, T2, A]) = {
    val association = f(source)
    val newSource = (source, association.right)
    val newJoin = queries.Builder.buildJoin(None, association)

    new Query2(newSource, select, newJoin, where)
  }

  def toSql() = new QueryBuilder(select, join, where).toSql

  def result = {
    val sql = new QueryBuilder(select, join, where).toSql
    val rs = Database.connection.execute(sql)

    new ResultSetMapper(source).map(rs)
  }
}

object Query {

  def apply[T1 <: Table](t: T1) = {
    val tableSelects = Seq(t.selectNode)
    val selectNode = SelectNode(tableSelects)

    new Query(t, selectNode, None, None)
  }
}
