package clunk.queries

import clunk.Association
import clunk.Ast.Node._
import clunk.jdbc.Database
import clunk.Mapping
import clunk.sql.QueryBuilder
import clunk.Table

class Select[T1 <: Table](
  val source: T1,
  val selectNode: SelectNode,
  val joinNode: Option[JoinNode],
  val whereNode: Option[WhereNode]) {

  def where(f: T1 => Comparison[_, _]) = {
    val newWhere = Builder.buildWhere(whereNode, f(source))
    new Select(source, selectNode, joinNode, newWhere)
  }

  def innerJoin[T2 <: Table, A](f: T1 => Association[T1, T2, A]) = {
    val association = f(source)
    val newSource = (source, association.right)
    val newJoin = Builder.buildJoin(None, association)

    new Select2(newSource, selectNode, newJoin, whereNode)
  }

  def result = {
    val builder = new QueryBuilder(selectNode, joinNode, whereNode)
    val mapFn = (Mapping.map(source) _)

    Database.connection(_.query(builder.toSql, builder.toParams, mapFn))
  }
}
