package clunk.converters

import clunk.Convertible
import java.sql.ResultSet

class Converter4[A, B, C, D, Record](val t: Tuple4[A, B, C, D] => Record)
    extends Convertible[Record] {

  val arity = 4

  def fromDb(rs: ResultSet, offset: Int) = {
    val tuple = Tuple4[A, B, C, D](
      rs.getObject(offset + 1).asInstanceOf[A],
      rs.getObject(offset + 2).asInstanceOf[B],
      rs.getObject(offset + 3).asInstanceOf[C],
      rs.getObject(offset + 4).asInstanceOf[D]
    )
    t(tuple)
  }
}
