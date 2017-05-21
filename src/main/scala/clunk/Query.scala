package clunk

import clunk.Ast.Node._
import clunk.Ast.Node.Comparator._
import clunk.sql.QueryBuilder

class Query[A <: Table[_]](
  val source: A,
  val select: SelectNode,
  val where: Option[WhereNode]) {

  def filter(f: A => Comparison[_]) = {
    val newWhere = Query.buildWhere(where, f(source))
    new Query(source, select, newWhere)
  }

  def innerJoin[B <: Table[_], C](f: A => Association[A, B, C]) = {
    val association = f(source)
    val newSource = (source, association.right)
    new Query2(newSource, select, where)
  }

  def toSql() = new QueryBuilder(select, where).toSql
}

class Query2[A <: Table[_], B <: Table[_]](
  val source: Tuple2[A, B],
  val select: SelectNode,
  val where: Option[WhereNode]) {

  def filter(f: Tuple2[A, B] => Comparison[_]) = {
    val newWhere = Query.buildWhere(where, f(source))
    new Query2(source, select, newWhere)
  }

  def toSql() = new QueryBuilder(select, where).toSql
}

object Query {
  def apply[A <: Table[_]](t: A) = {
    val tableSelects = Seq(t.selectNode)
    val selectNode = SelectNode(tableSelects)

    new Query[A](t, selectNode, None)
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
