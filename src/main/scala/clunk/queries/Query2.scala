package clunk.queries

import clunk.Association
import clunk.Ast.Node._
import clunk.jdbc.Database
import clunk.mappers.ResultSetMapper2
import clunk.sql.QueryBuilder
import clunk.Table

class Query2[T1 <: Table, T2 <: Table](
  val source: Tuple2[T1, T2],
  val select: SelectNode,
  val join: Option[JoinNode],
  val where: Option[WhereNode]) {

  def filter(f: Tuple2[T1, T2] => Comparison[_]) = {
    val newWhere = Builder.buildWhere(where, f(source))
    new Query2(source, select, join, newWhere)
  }

  def innerJoin[T3 <: Table, T4 <: Table, A]
      (f: Tuple2[T1, T2] => Association[T3, T4, A]) = {

    val association = f(source)
    val newSource = (source._1, source._2, association.right)
    val newJoin = Builder.buildJoin(join, association)

    new Query3(newSource, select, newJoin, where)
  }

  def toSql() = new QueryBuilder(select, join, where).toSql

  def result = {
    val sql = new QueryBuilder(select, join, where).toSql
    val rs = Database.connection.execute(sql)

    new ResultSetMapper2(source).map(rs)
  }
}
