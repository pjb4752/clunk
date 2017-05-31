package clunk

import clunk.Ast.Node._
import clunk.Ast.Node.Comparator._
import clunk.jdbc.Database
import clunk.mappers.ResultSetMapper
import clunk.queries.Query2
import clunk.sql.QueryBuilder

class Query[A <: Table[_]](
  val source: A,
  val select: SelectNode,
  val join: Option[JoinNode],
  val where: Option[WhereNode]) {

  def filter(f: A => Comparison[_]) = {
    val newWhere = queries.Builder.buildWhere(where, f(source))
    new Query(source, select, join, newWhere)
  }

  def innerJoin[B <: Table[_], C](f: A => Association[A, B, C]) = {
    val association = f(source)
    val newSource = (source, association.right)
    val newJoin = queries.Builder.buildJoin(None, association)

    new Query2(newSource, select, newJoin, where)
  }

  def toSql() = new QueryBuilder(select, join, where).toSql

  def result = {
    val sql = new QueryBuilder(select, join, where).toSql
    val rs = Database.connection.execute(sql)

    new ResultSetMapper[A](source).map(rs)
  }
}

object Query {

  def apply[A <: Table[_]](t: A) = {
    val tableSelects = Seq(t.selectNode)
    val selectNode = SelectNode(tableSelects)

    new Query[A](t, selectNode, None, None)
  }
}
