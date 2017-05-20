package clunk

import clunk.Ast.Node._
import clunk.Ast.Node.Comparator._

class Query[A <: Table[_]](val baseTable: A, val select: SelectNode,
  val where: Option[WhereNode]) {

  def filter(f: A => Comparison[_]) = {
    val prevComparisons = where.map(_.comparisons).
      getOrElse(Seq[Comparison[_]]())
    val newComparisons = prevComparisons :+ f(baseTable)
    val newWhere = Some(WhereNode(newComparisons))

    new Query[A](this.baseTable, this.select, newWhere)
  }

  def toSql() = new SqlBuilder(baseTable.srcName, select, where).toSql
}

object Query {
  def apply[A <: Table[_]](t: A) = new Query[A](t, t.selectNode, None)
}
