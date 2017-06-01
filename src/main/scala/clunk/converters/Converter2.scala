package clunk.converters

import clunk.Convertible
import java.sql.ResultSet

class Converter2[A, B, Record](val t: Tuple2[A, B] => Record)
    extends Convertible[Record] {

  val arity = 2

  def fromDb(rs: ResultSet, offset: Int) = {
    val tuple = Tuple2[A, B](
      rs.getObject(offset + 1).asInstanceOf[A],
      rs.getObject(offset + 2).asInstanceOf[B]
    )
    t(tuple)
  }
}

