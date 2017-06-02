package clunk.queries

import clunk.Association
import clunk.Ast.Node._
import clunk.jdbc.Database
import clunk.Mapping
import clunk.sql.QueryBuilder
import clunk.Table

class Query3[T1 <: Table, T2 <: Table, T3 <: Table](
  val source: Tuple3[T1, T2, T3],
  val select: SelectNode,
  val join: Option[JoinNode],
  val where: Option[WhereNode]) {

  def filter(f: Tuple3[T1, T2, T3] => Comparison[_]) = {
    val newWhere = Builder.buildWhere(where, f(source))
    new Query3(source, select, join, newWhere)
  }

  def innerJoin[T4 <: Table, T5 <: Table, A]
      (f: Tuple3[T1, T2, T3] => Association[T4, T5, A]) = {

    val association = f(source)
    val newSource = (source._1, source._2, source._3, association.right)
    val newJoin = Builder.buildJoin(join, association)

    new Query4(newSource, select, newJoin, where)
  }

  def toSql() = new QueryBuilder(select, join, where).toSql

  def result = {
    val sql = new QueryBuilder(select, join, where).toSql
    val rs = Database.connection.execute(sql)

    Mapping.map3(source, rs)
  }
}
