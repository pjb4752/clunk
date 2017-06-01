package clunk.converters

import clunk.Convertible
import java.sql.ResultSet

class Converter3[A, B, C, Record](val t: Tuple3[A, B, C] => Record)
    extends Convertible[Record] {

  val arity = 3

  def fromDb(rs: ResultSet, offset: Int) = {
    val tuple = Tuple3[A, B, C](
      rs.getObject(offset + 1).asInstanceOf[A],
      rs.getObject(offset + 2).asInstanceOf[B],
      rs.getObject(offset + 3).asInstanceOf[C]
    )
    t(tuple)
  }
}
