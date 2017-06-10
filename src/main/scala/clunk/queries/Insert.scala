package clunk.queries

import clunk.{Mapping, Table}
import clunk.jdbc.Database
import clunk.sql.InsertBuilder

class Insert {

  def execute(source: Table)(record: source.Record) = {
    val columns = source.columns.toSeq
    val maybeProduct = Mapping.unmap(source)(record)
    val insertSql = new InsertBuilder(source).toSql

    Database.connection(_.insert(insertSql, columns, maybeProduct))
  }
}
