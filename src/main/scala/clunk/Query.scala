package clunk

import clunk.Ast.Node._
import clunk.Ast.Node.Comparator._
import clunk.jdbc.Database
import clunk.queries.Query2
import clunk.sql.QueryBuilder

class Query[T1 <: Table](
  val source: T1,
  val selectNode: SelectNode,
  val joinNode: Option[JoinNode],
  val whereNode: Option[WhereNode]) {

  def where(f: T1 => Comparison[_]) = {
    val newWhere = queries.Builder.buildWhere(whereNode, f(source))
    new Query(source, selectNode, joinNode, newWhere)
  }

  def innerJoin[T2 <: Table, A](f: T1 => Association[T1, T2, A]) = {
    val association = f(source)
    val newSource = (source, association.right)
    val newJoin = queries.Builder.buildJoin(None, association)

    new Query2(newSource, selectNode, newJoin, whereNode)
  }

  def toSql() = new QueryBuilder(selectNode, joinNode, whereNode).toSql

  def result = {
    val sql = new QueryBuilder(selectNode, joinNode, whereNode).toSql
    val rs = Database.connection.execute(sql)

    Mapping.map(source, rs)
  }
}

object Query {

  def apply[T1 <: Table](t: T1) = {
    val tableSelects = Seq(t.selectNode)
    val selectNode = SelectNode(tableSelects)

    new Query(t, selectNode, None, None)
  }
}
