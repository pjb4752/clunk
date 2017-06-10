package clunk.sql

import clunk.{Column, Table}

class UpdateBuilder(destTable: Table, fieldValues: Seq[(Column[_, _], Any)],
    keyValues: Seq[(Column[_, _], Any)]) {

  def toSql = baseSql + whereSql

  private def baseSql = fieldValues.map({
      case (c, _) => s"`${c.srcName}` = ?"
    }).mkString(s"UPDATE `${destTable.srcName}` SET\n", ",\n", "\n WHERE")

  private def whereSql = keyValues.map({
      case (c, _) => s"`${c.srcName}` = ?"
    }).mkString(" AND ")
}
