package clunk.sql

import clunk.Table

class InsertBuilder(destTable: Table) {

  def toSql = baseSql + bindSql

  private def baseSql = nonGenColumns.map({ c => s"`${c.srcName}`" }).
      mkString(s"INSERT INTO `${destTable.srcName}` (", ", ", ")")

  private def bindSql = nonGenColumns.map({ c => "?" }).
      mkString(" VALUES (", ", ", ")")

  private def nonGenColumns = destTable.columns.filterNot(_.isAutoGen)
}
