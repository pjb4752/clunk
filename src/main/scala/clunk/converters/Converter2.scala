package clunk.converters

import clunk.Convertible
import java.sql.ResultSet

class Converter2[B, C, A](val t: Tuple2[B, C] => A) extends Convertible[A] {
  val arity = 2

  def fromDb(rs: ResultSet, offset: Int) = {
    val tuple = Tuple2[B, C](
      rs.getObject(offset + 1).asInstanceOf[B],
      rs.getObject(offset + 2).asInstanceOf[C]
    )
    t(tuple)
  }
}

