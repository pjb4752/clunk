package clunk.converters

import clunk.Convertible
import java.sql.ResultSet

class Converter7[A, B, C, D, E, F, G, Record]
    (val t: Tuple7[A, B, C, D, E, F, G] => Record)
    extends Convertible[Record] {

  val arity = 7

  def fromDb(rs: ResultSet, offset: Int) = {
    val tuple = Tuple7[A, B, C, D, E, F, G](
      rs.getObject(offset + 1).asInstanceOf[A],
      rs.getObject(offset + 2).asInstanceOf[B],
      rs.getObject(offset + 3).asInstanceOf[C],
      rs.getObject(offset + 4).asInstanceOf[D],
      rs.getObject(offset + 5).asInstanceOf[E],
      rs.getObject(offset + 6).asInstanceOf[F],
      rs.getObject(offset + 7).asInstanceOf[G]
    )
    t(tuple)
  }
}
