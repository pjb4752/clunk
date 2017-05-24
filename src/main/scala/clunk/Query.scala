package clunk

import clunk.Ast.Node._
import clunk.Ast.Node.Comparator._
import clunk.sql.QueryBuilder

class Query[A <: Table[_]](
  val source: A,
  val select: SelectNode,
  val join: Option[JoinNode],
  val where: Option[WhereNode]) {

  def filter(f: A => Comparison[_]) = {
    val newWhere = Query.buildWhere(where, f(source))
    new Query(source, select, join, newWhere)
  }

  def innerJoin[B <: Table[_], C](f: A => Association[A, B, C]) = {
    val association = f(source)
    val newSource = (source, association.right)
    val newJoin = Query.buildJoin(None, association)

    new Query2(newSource, select, newJoin, where)
  }

  def toSql() = new QueryBuilder(select, join, where).toSql
}

class Query2[A <: Table[_], B <: Table[_]](
  val source: Tuple2[A, B],
  val select: SelectNode,
  val join: Option[JoinNode],
  val where: Option[WhereNode]) {

  def filter(f: Tuple2[A, B] => Comparison[_]) = {
    val newWhere = Query.buildWhere(where, f(source))
    new Query2(source, select, join, newWhere)
  }

  def innerJoin[C <: Table[_], D <: Table[_], E]
      (f: Tuple2[A, B] => Association[C, D, E]) = {

    val association = f(source)
    val newSource = (source._1, source._2, association.right)
    val newJoin = Query.buildJoin(join, association)

    new Query3(newSource, select, newJoin, where)
  }

  def toSql() = new QueryBuilder(select, join, where).toSql
}

class Query3[A <: Table[_], B <: Table[_], C <: Table[_]](
  val source: Tuple3[A, B, C],
  val select: SelectNode,
  val join: Option[JoinNode],
  val where: Option[WhereNode]) {

  def filter(f: Tuple3[A, B, C] => Comparison[_]) = {
    val newWhere = Query.buildWhere(where, f(source))
    new Query3(source, select, join, newWhere)
  }

  def toSql() = new QueryBuilder(select, join, where).toSql
}

object Query {
  def apply[A <: Table[_]](t: A) = {
    val tableSelects = Seq(t.selectNode)
    val selectNode = SelectNode(tableSelects)

    new Query[A](t, selectNode, None, None)
  }

  def buildJoin[A <: Table[_], B <: Table[_]]
      (oldJoin: Option[JoinNode], association: Association[A, B, _]) = {

    val prevJoins = oldJoin.map(_.tableJoins).getOrElse(Seq[TableJoinNode]())
    val newJoins = prevJoins :+ TableJoinNode(association.left,
      association.right, association.fk, association.pk)

    Some(JoinNode(newJoins))
  }

  def buildWhere(oldWhere: Option[WhereNode], comparison: Comparison[_]) = {
    val table = comparison.column.table

    val maybeFilters = oldWhere.map(_.tableFilters)
    val maybeFilter = maybeFilters.flatMap(_.find(_.table == table))

    val prevComparisons = maybeFilter.map(_.comparisons).
      getOrElse(Seq[Comparison[_]]())
    val newTableNode = TableWhereNode(table, prevComparisons :+ comparison)

    val tableNodes = maybeFilters.map(_.filterNot(_.table == table)).
      getOrElse(Seq[TableWhereNode]()) :+ newTableNode

    Some(WhereNode(tableNodes))
  }
}
