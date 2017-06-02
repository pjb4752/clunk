package clunk.queries

import clunk.Association
import clunk.Ast.Node._
import clunk.jdbc.Database
import clunk.Mapping
import clunk.sql.QueryBuilder
import clunk.Table

class Query4[T1 <: Table, T2 <: Table, T3 <: Table, T4 <: Table](
  val source: Tuple4[T1, T2, T3, T4],
  val select: SelectNode,
  val join: Option[JoinNode],
  val where: Option[WhereNode]) {

  def filter(f: Tuple4[T1, T2, T3, T4] => Comparison[_]) = {
    val newWhere = Builder.buildWhere(where, f(source))
    new Query4(source, select, join, newWhere)
  }

  def innerJoin[T5 <: Table, T6 <: Table, A]
      (f: Tuple4[T1, T2, T3, T4] => Association[T5, T6, A]) = {

    val association = f(source)
    val newSource = (source._1, source._2, source._3, source._4, association.right)
    val newJoin = Builder.buildJoin(join, association)

    new Query5(newSource, select, newJoin, where)
  }

  def toSql() = new QueryBuilder(select, join, where).toSql

  def result = {
    val sql = new QueryBuilder(select, join, where).toSql
    val rs = Database.connection.execute(sql)

    Mapping.map4(source, rs)
  }
}
