package clunk.converters

import clunk.Convertible
import java.sql.ResultSet

class Converter5[A, B, C, D, E, Record](val t: Tuple5[A, B, C, D, E] => Record)
    extends Convertible[Record] {

  val arity = 5

  def fromDb(rs: ResultSet, offset: Int) = {
    val tuple = Tuple5[A, B, C, D, E](
      rs.getObject(offset + 1).asInstanceOf[A],
      rs.getObject(offset + 2).asInstanceOf[B],
      rs.getObject(offset + 3).asInstanceOf[C],
      rs.getObject(offset + 4).asInstanceOf[D],
      rs.getObject(offset + 5).asInstanceOf[E]
    )
    t(tuple)
  }
}
