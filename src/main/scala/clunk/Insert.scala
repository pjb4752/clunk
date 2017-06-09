package clunk

import clunk.jdbc.Database

class Insert[T1 <: Table](val source: T1) {

  def execute(record: source.Record): Int = {
    val columns = source.columns.toSeq
    val maybeProduct = Mapping.unmap(source)(record)

    Database.connection(_.insert(insertSql, columns, maybeProduct))
  }

  private def insertSql = {
    val nonAutoColumns = source.columns.filterNot(_.isAutoGen)

    val baseSql = nonAutoColumns.map({ c => s"`${c.srcName}`" }).
      mkString(s"INSERT INTO `${source.srcName}` (", ", ", ")")
    val bindSql = nonAutoColumns.map({ c => "?" }).
      mkString(" VALUES (", ", ", ")")

    baseSql + bindSql
  }
}

object Insert {
  def apply[T1 <: Table](t: T1) = new Insert(t)
}
