package clunk.sql

import clunk.Ast.Node._
import clunk.Ast.Node.Comparator._
import clunk.TypeTag._

class SelectBuilder(select: SelectNode, join: Option[JoinNode]) {

  def toSql(builder: StringBuilder) = {
    baseSelect(builder)
    join.map({ j =>
      if (j.tableJoins.isEmpty) builder
      else {
        builder.append(", ")
        joinSelect(builder, j.tableJoins)
      }
    }).getOrElse(builder)
  }

  private def baseSelect(builder: StringBuilder) = {
    select.tableSelects.flatMap({ ts =>
      ts.columns.map({ c => s"`${ts.table.srcName}`.`${c.srcName}`" })
    }).addString(builder, "SELECT ", ", ", "")
  }

  private def joinSelect(builder: StringBuilder, joins: Seq[TableJoinNode]) = {
    joins.flatMap({ j =>
      j.right.columns.map({ c => s"`${j.right.srcName}`.`${c.srcName}`" })
    }).addString(builder, ", ")
  }
}
