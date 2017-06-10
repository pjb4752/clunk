package clunk.queries

import clunk.{Mapping, Table}
import clunk.jdbc.Database
import clunk.sql.UpdateBuilder

class Update {

  def execute(source: Table)(record: source.Record) = {
    val primaryKeys = source.primaryKeys
    val (pks, fields) = source.columns.zipWithIndex.partition({
        case (c, i) => primaryKeys.contains(c) })
    val maybeProduct = Mapping.unmap(source)(record)

    maybeProduct.map({ p =>
      val fieldValues = fields.map({ case (c, i) => (c, p.productElement(i)) })
      val keyValues = pks.map({ case (c, i) => (c, p.productElement(i)) })
      val sql = new UpdateBuilder(source, fieldValues, keyValues).toSql

      Database.connection(_.update(sql, fieldValues, keyValues))
    }).getOrElse(0)
  }
}
