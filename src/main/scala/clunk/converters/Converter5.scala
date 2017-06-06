package clunk.converters

import clunk.{Column, Convertible}
import java.sql.ResultSet

class Converter5[A, B, C, D, E, Record](val t: Tuple5[A, B, C, D, E] => Record)
    extends Convertible[Record] {

  val arity = 5

  def fromDb(columns: Seq[Column[_, _]], rs: ResultSet, offset: Int) = {
    val t1 = Conversion.getValue[A](columns(0), rs, offset + 1)
    val t2 = Conversion.getValue[B](columns(1), rs, offset + 2)
    val t3 = Conversion.getValue[C](columns(2), rs, offset + 3)
    val t4 = Conversion.getValue[D](columns(3), rs, offset + 4)
    val t5 = Conversion.getValue[E](columns(4), rs, offset + 5)
    val tuple = Tuple5[A, B, C, D, E](t1, t2, t3, t4, t5)
    t(tuple)
  }
}
