package clunk.queries

import clunk.Association
import clunk.Ast.Node._
import clunk.Ast.Node.Comparator._
import clunk.Table

object Builder {

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
