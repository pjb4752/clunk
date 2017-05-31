package clunk.queries

import clunk.Association
import clunk.Ast.Node._
import clunk.jdbc.Database
import clunk.mappers.ResultSetMapper2
import clunk.sql.QueryBuilder
import clunk.Table

class Query2[A <: Table[_], B <: Table[_]](
  val source: Tuple2[A, B],
  val select: SelectNode,
  val join: Option[JoinNode],
  val where: Option[WhereNode]) {

  def filter(f: Tuple2[A, B] => Comparison[_]) = {
    val newWhere = Builder.buildWhere(where, f(source))
    new Query2(source, select, join, newWhere)
  }

  def innerJoin[C <: Table[_], D <: Table[_], E]
      (f: Tuple2[A, B] => Association[C, D, E]) = {

    val association = f(source)
    val newSource = (source._1, source._2, association.right)
    val newJoin = Builder.buildJoin(join, association)

    new Query3(newSource, select, newJoin, where)
  }

  def toSql() = new QueryBuilder(select, join, where).toSql

  def result = {
    val sql = new QueryBuilder(select, join, where).toSql
    val rs = Database.connection.execute(sql)

    new ResultSetMapper2[A, B](source).map(rs)
  }
}
