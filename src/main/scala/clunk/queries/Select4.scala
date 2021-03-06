package clunk.queries

import clunk.Association
import clunk.Ast.Node._
import clunk.jdbc.Database
import clunk.Mapping
import clunk.sql.QueryBuilder
import clunk.Table

class Select4[T1 <: Table, T2 <: Table, T3 <: Table, T4 <: Table](
  val source: Tuple4[T1, T2, T3, T4],
  val selectNode: SelectNode,
  val joinNode: Option[JoinNode],
  val whereNode: Option[WhereNode]) {

  def where(f: Tuple4[T1, T2, T3, T4] => Comparison[_, _]) = {
    val newWhere = Builder.buildWhere(whereNode, f(source))
    new Select4(source, selectNode, joinNode, newWhere)
  }

  def innerJoin[T5 <: Table, T6 <: Table, A]
      (f: Tuple4[T1, T2, T3, T4] => Association[T5, T6, A]) = {

    val association = f(source)
    val newSource = (source._1, source._2, source._3, source._4, association.right)
    val newJoin = Builder.buildJoin(joinNode, association)

    new Select5(newSource, selectNode, newJoin, whereNode)
  }

  def toSql() = new QueryBuilder(selectNode, joinNode, whereNode).toSql

  def result = {
    val builder = new QueryBuilder(selectNode, joinNode, whereNode)
    val mapFn = (Mapping.map4(source) _)

    Database.connection(_.query(builder.toSql, builder.toParams, mapFn))
  }
}
