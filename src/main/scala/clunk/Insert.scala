package clunk

import clunk.jdbc.Database

class Insert[T1 <: Table](val source: T1) {

  def execute(record: source.Record): Int = {
    source.projectionArity match {
      case 2 => execute2(record)
      case 3 => execute3(record)
      case 4 => execute4(record)
    }
  }

  private def insertSql = {
    val nonAutoColumns = source.columns.filterNot(_.isAutoGen)

    val baseSql = nonAutoColumns.map(_.srcName).
      mkString(s"INSERT INTO `${source.srcName}` (", ", ", ")")
    val bindSql = nonAutoColumns.map({ c => "?" }).
      mkString(" VALUES (", ", ", ")")

    baseSql + bindSql
  }

  private def columns = source.columns.toSeq

  private def execute2(record: source.Record) = {
    val maybeTuple = Mapping.unmap2(source)(record)
    Database.connection(_.insert2(insertSql, columns, maybeTuple))
  }

  private def execute3(record: source.Record) = {
    val maybeTuple = Mapping.unmap3(source)(record)
    Database.connection(_.insert3(insertSql, columns, maybeTuple))
  }

  private def execute4(record: source.Record) = {
    val maybeTuple = Mapping.unmap4(source)(record)
    Database.connection(_.insert4(insertSql, columns, maybeTuple))
  }
}

object Insert {

  def apply[T1 <: Table](t: T1) = new Insert(t)
}
