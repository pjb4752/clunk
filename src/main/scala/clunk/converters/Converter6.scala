package clunk.converters

import clunk.Convertible
import java.sql.ResultSet

class Converter6[A, B, C, D, E, F, Record]
    (val t: Tuple6[A, B, C, D, E, F] => Record) extends Convertible[Record] {

  val arity = 6

  def fromDb(rs: ResultSet, offset: Int) = {
    val tuple = Tuple6[A, B, C, D, E, F](
      rs.getObject(offset + 1).asInstanceOf[A],
      rs.getObject(offset + 2).asInstanceOf[B],
      rs.getObject(offset + 3).asInstanceOf[C],
      rs.getObject(offset + 4).asInstanceOf[D],
      rs.getObject(offset + 5).asInstanceOf[E],
      rs.getObject(offset + 6).asInstanceOf[F]
    )
    t(tuple)
  }
}
