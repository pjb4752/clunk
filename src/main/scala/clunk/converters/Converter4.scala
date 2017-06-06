package clunk.converters

import clunk.{Column, Convertible}
import java.sql.ResultSet

class Converter4[A, B, C, D, Record](val t: Tuple4[A, B, C, D] => Record)
    extends Convertible[Record] {

  val arity = 4

  def fromDb(columns: Seq[Column[_, _]], rs: ResultSet, offset: Int) = {
    val t1 = Conversion.getValue[A](columns(0), rs, offset + 1)
    val t2 = Conversion.getValue[B](columns(1), rs, offset + 2)
    val t3 = Conversion.getValue[C](columns(2), rs, offset + 3)
    val t4 = Conversion.getValue[D](columns(3), rs, offset + 4)
    val tuple = Tuple4[A, B, C, D](t1, t2, t3, t4)
    t(tuple)
  }
}
