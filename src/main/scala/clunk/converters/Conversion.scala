package clunk.converters

import clunk.Column
import java.sql.ResultSet

object Conversion {

  def getValue[A](column: Column[_, _], rs : ResultSet, i: Int): A =
    if (column.isNullable) getOptionalValue[A](rs, i)
    else rs.getObject(i).asInstanceOf[A]

  def getOptionalValue[A](rs: ResultSet, i: Int): A = {
    val dbValue = rs.getObject(i)

    if (dbValue != null) {
      Some(dbValue).asInstanceOf[A]
    } else {
      None.asInstanceOf[A]
    }
  }
}
