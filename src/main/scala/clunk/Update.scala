package clunk

import clunk.jdbc.Database

class Update[T1 <: Table](val source: T1) {

  def execute(record: source.Record): Int = {
    val primaryKeys = source.primaryKeys
    val (pks, fields) = source.columns.zipWithIndex.partition({
        case (c, i) => primaryKeys.contains(c) })
    val maybeProduct = Mapping.unmap(source)(record)

    maybeProduct.map({ p =>
      val pkBindings = pks.map({ case (c, i) => (c, p.productElement(i)) })
      val fBindings = fields.map({ case (c, i) => (c, p.productElement(i)) })
      val sql = updateSql(fBindings, pkBindings)

      Database.connection(_.update(sql, fBindings, pkBindings))
    }).getOrElse(0)
  }

  private def updateSql(fieldBindings: Seq[Tuple2[Column[_, _], Any]],
      pkBindings: Seq[Tuple2[Column[_, _], Any]]) = {

    val baseSql = fieldBindings.map({ case (c, i) => s"`${c.srcName}` = ?" }).
        mkString(s"UPDATE `${source.srcName}` SET\n", ",\n", "\n WHERE")
    val whereClause = pkBindings.map({ case (c, i) => s"`${c.srcName}` = ?" }).
        mkString(" AND ")

    baseSql + whereClause
  }
}

object Update {
  def apply[T1 <: Table](t: T1) = new Update(t)
}
