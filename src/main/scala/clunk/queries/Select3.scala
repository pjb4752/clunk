package clunk.queries

import clunk.Association
import clunk.Ast.Node._
import clunk.jdbc.Database
import clunk.Mapping
import clunk.sql.QueryBuilder
import clunk.Table

class Select3[T1 <: Table, T2 <: Table, T3 <: Table](
  val source: Tuple3[T1, T2, T3],
  val selectNode: SelectNode,
  val joinNode: Option[JoinNode],
  val whereNode: Option[WhereNode]) {

  def where(f: Tuple3[T1, T2, T3] => Comparison[_, _]) = {
    val newWhere = Builder.buildWhere(whereNode, f(source))
    new Select3(source, selectNode, joinNode, newWhere)
  }

  def innerJoin[T4 <: Table, T5 <: Table, A]
      (f: Tuple3[T1, T2, T3] => Association[T4, T5, A]) = {

    val association = f(source)
    val newSource = (source._1, source._2, source._3, association.right)
    val newJoin = Builder.buildJoin(joinNode, association)

    new Select4(newSource, selectNode, newJoin, whereNode)
  }

  def toSql() = new QueryBuilder(selectNode, joinNode, whereNode).toSql

  def result = {
    val builder = new QueryBuilder(selectNode, joinNode, whereNode)
    val mapFn = (Mapping.map3(source) _)

    Database.connection(_.query(builder.toSql, builder.toParams, mapFn))
  }
}
