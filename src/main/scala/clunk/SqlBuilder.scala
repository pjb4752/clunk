package clunk

import clunk.Ast.Node._
import clunk.Ast.Node.Comparator._
import clunk.TypeTag._

class SqlBuilder(table: String, select: SelectNode, where: Option[WhereNode]) {
  val builder = new StringBuilder()

  def toSql() = {
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
    builder.append(s" FROM `${table}`")
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
