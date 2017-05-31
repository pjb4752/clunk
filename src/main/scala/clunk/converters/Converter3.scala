package clunk.converters

import clunk.Convertible
import java.sql.ResultSet

class Converter3[B, C, D, A](val t: Tuple3[B, C, D] => A) extends Convertible[A] {
  val arity = 3

  def fromDb(rs: ResultSet, offset: Int) = {
    val tuple = Tuple3[B, C, D](
      rs.getObject(offset + 1).asInstanceOf[B],
      rs.getObject(offset + 2).asInstanceOf[C],
      rs.getObject(offset + 3).asInstanceOf[D]
    )
    t(tuple)
  }
}
