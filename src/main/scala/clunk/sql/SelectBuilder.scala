package clunk.sql

import clunk.Ast.Node._
import clunk.Ast.Node.Comparator._
import clunk.TypeTag._

class SelectBuilder(select: SelectNode) {

  def toSql(builder: StringBuilder) = {
    select.tableSelects.flatMap({ ts =>
      ts.columns.map({ c => s"`${ts.table.srcName}`.`${c.srcName}`" })
    }).addString(builder, "SELECT ", ", ", "")
  }
}
