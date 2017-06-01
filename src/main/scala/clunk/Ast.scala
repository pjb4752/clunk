package clunk

import scala.collection.mutable.ArrayBuffer

object Ast {

  sealed trait Node

  object Node {

    sealed trait Comparator

    object Comparator {
      case object EqualTo extends Comparator
      case object LessThan extends Comparator
      case object MoreThan extends Comparator
    }

    class Comparison[A](
      val comparator: Comparator,
      val column: Column[A],
      val value: A)

    case class TableSelectNode(
      val table: Table,
      val columns: Seq[Column[_]]) extends Node

    case class TableJoinNode(
      val left: Table,
      val right: Table,
      val fk: Column[_],
      val pk: Column[_])

    case class TableWhereNode(
      val table: Table,
      val comparisons: Seq[Comparison[_]]) extends Node

    case class SelectNode(val tableSelects: Seq[TableSelectNode]) extends Node
    case class JoinNode(val tableJoins: Seq[TableJoinNode]) extends Node
    case class WhereNode(val tableFilters: Seq[TableWhereNode]) extends Node
  }
}
