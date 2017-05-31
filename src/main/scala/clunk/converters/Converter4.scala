package clunk.converters

import clunk.Convertible
import java.sql.ResultSet

class Converter4[B, C, D, E, A](val t: Tuple4[B, C, D, E] => A)
  extends Convertible[A] {

  val arity = 4

  def fromDb(rs: ResultSet, offset: Int) = {
    val tuple = Tuple4[B, C, D, E](
      rs.getObject(offset + 1).asInstanceOf[B],
      rs.getObject(offset + 2).asInstanceOf[C],
      rs.getObject(offset + 3).asInstanceOf[D],
      rs.getObject(offset + 4).asInstanceOf[E]
    )
    t(tuple)
  }
}
