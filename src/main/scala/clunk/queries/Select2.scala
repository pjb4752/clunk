package clunk.queries

import clunk.Association
import clunk.Ast.Node._
import clunk.jdbc.Database
import clunk.Mapping
import clunk.sql.QueryBuilder
import clunk.Table

class Select2[T1 <: Table, T2 <: Table](
  val source: Tuple2[T1, T2],
  val selectNode: SelectNode,
  val joinNode: Option[JoinNode],
  val whereNode: Option[WhereNode]) {

  def where(f: Tuple2[T1, T2] => Comparison[_, _]) = {
    val newWhere = Builder.buildWhere(whereNode, f(source))
    new Select2(source, selectNode, joinNode, newWhere)
  }

  def innerJoin[T3 <: Table, T4 <: Table, A]
      (f: Tuple2[T1, T2] => Association[T3, T4, A]) = {

    val association = f(source)
    val newSource = (source._1, source._2, association.right)
    val newJoin = Builder.buildJoin(joinNode, association)

    new Select3(newSource, selectNode, newJoin, whereNode)
  }

  def toSql() = new QueryBuilder(selectNode, joinNode, whereNode).toSql

  def result = {
    val builder = new QueryBuilder(selectNode, joinNode, whereNode)
    val mapFn = (Mapping.map2(source) _)

    Database.connection(_.query(builder.toSql, builder.toParams, mapFn))
  }
}
