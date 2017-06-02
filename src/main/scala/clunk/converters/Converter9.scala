package clunk.converters

import clunk.Convertible
import java.sql.ResultSet

class Converter9[A, B, C, D, E, F, G, H, I, Record]
    (val t: Tuple9[A, B, C, D, E, F, G, H, I] => Record)
    extends Convertible[Record] {

  val arity = 8

  def fromDb(rs: ResultSet, offset: Int) = {
    val tuple = Tuple9[A, B, C, D, E, F, G, H, I](
      rs.getObject(offset + 1).asInstanceOf[A],
      rs.getObject(offset + 2).asInstanceOf[B],
      rs.getObject(offset + 3).asInstanceOf[C],
      rs.getObject(offset + 4).asInstanceOf[D],
      rs.getObject(offset + 5).asInstanceOf[E],
      rs.getObject(offset + 6).asInstanceOf[F],
      rs.getObject(offset + 7).asInstanceOf[G],
      rs.getObject(offset + 8).asInstanceOf[H],
      rs.getObject(offset + 9).asInstanceOf[I]
    )
    t(tuple)
  }
}