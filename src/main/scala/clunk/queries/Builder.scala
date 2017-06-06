package clunk.queries

import clunk.Association
import clunk.Ast.Node._
import clunk.Ast.Node.Comparator._
import clunk.Table

object Builder {

  def buildJoin[T1 <: Table, T2 <: Table]
      (oldJoin: Option[JoinNode], association: Association[T1, T2, _]) = {

    val prevJoins = oldJoin.map(_.tableJoins).getOrElse(Seq[TableJoinNode]())
    val newJoins = prevJoins :+ TableJoinNode(association.left,
      association.right, association.fk, association.pk)

    Some(JoinNode(newJoins))
  }

  def buildWhere(oldWhere: Option[WhereNode], comparison: Comparison[_, _]) = {
    val table = comparison.column.table

    val maybeFilters = oldWhere.map(_.tableFilters)
    val maybeFilter = maybeFilters.flatMap(_.find(_.table == table))

    val prevComparisons = maybeFilter.map(_.comparisons).
      getOrElse(Seq[Comparison[_, _]]())
    val newTableNode = TableWhereNode(table, prevComparisons :+ comparison)

    val tableNodes = maybeFilters.map(_.filterNot(_.table == table)).
      getOrElse(Seq[TableWhereNode]()) :+ newTableNode

    Some(WhereNode(tableNodes))
  }
}
