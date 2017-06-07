package clunk.queries

import clunk.Ast.Node._
import clunk.jdbc.Database
import clunk.Mapping
import clunk.sql.QueryBuilder
import clunk.Table

class Query5[T1 <: Table, T2 <: Table, T3 <: Table, T4 <: Table, T5 <: Table](
  val source: Tuple5[T1, T2, T3, T4, T5],
  val selectNode: SelectNode,
  val joinNode: Option[JoinNode],
  val whereNode: Option[WhereNode]) {

  def where(f: Tuple5[T1, T2, T3, T4, T5] => Comparison[_, _]) = {
    val newWhere = Builder.buildWhere(whereNode, f(source))
    new Query5(source, selectNode, joinNode, newWhere)
  }

  def toSql() = new QueryBuilder(selectNode, joinNode, whereNode).toSql

  def result = {
    val builder = new QueryBuilder(selectNode, joinNode, whereNode)
    val mapFn = (Mapping.map5(source) _)

    Database.connection(_.query(builder.toSql, builder.toParams, mapFn))
  }
}
