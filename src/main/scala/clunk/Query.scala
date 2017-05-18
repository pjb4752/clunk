package clunk

import clunk.Ast.Node._
import clunk.Ast.Node.Comparator._
import clunk.TypeTag._

class Query[A <: Table[_]](val baseTable: A, val select: SelectNode,
  val where: Option[WhereNode]) {

  def filter(f: A => Comparison[_]) = {
    val prevComparisons = where.map(_.comparisons).
      getOrElse(Seq[Comparison[_]]())
    val newComparisons = prevComparisons :+ f(baseTable)
    val newWhere = Some(WhereNode(newComparisons))

    new Query[A](this.baseTable, this.select, newWhere)
  }

  def toSql() = {
    val builder = new StringBuilder()

    buildSelect(builder)
    buildFrom(builder)
    buildWhere(builder)

    builder.toString()
  }

  private def buildSelect(builder: StringBuilder): Unit = {
    builder.append("SELECT ")
    select.columns.map({ c => s"`${c.srcName}`" }).addString(builder, ", ")
  }

  private def buildFrom(builder: StringBuilder): Unit = {
    builder.append(s" FROM `${baseTable.srcName}`")
  }

  private def buildWhere(builder: StringBuilder): Unit = {
    where.map(_.comparisons).map({ cmps =>
      cmps.map({ c =>
        builder.append(" WHERE ")
        c.comparator match {
          case EqualTo => buildComparison("=", c.column, c.value)
          case LessThan => buildComparison("<", c.column, c.value)
          case MoreThan => buildComparison(">", c.column, c.value)
        }
      }).addString(builder, " AND ")
    }).getOrElse("")
  }

  private def buildComparison(op: String, c: Column[_], v: Any) = {
    val value = c.typeTag match {
      case StrTag => s"'${v}'"
      case _      => s"${v}"
    }

    s"`${c.srcName}` ${op} ${value}"
  }
}

object Query {
  def apply[A <: Table[_]](t: A) = new Query[A](t, t.selectNode, None)
}
