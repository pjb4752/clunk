package clunk.converters

import clunk.{Column, Convertible}
import java.sql.ResultSet

class Converter8[A, B, C, D, E, F, G, H, Record]
    (val t: Tuple8[A, B, C, D, E, F, G, H] => Record)
    extends Convertible[Record] {

  val arity = 8

  def fromDb(columns: Seq[Column[_, _]], rs: ResultSet, offset: Int) = {
    val t1 = Conversion.getValue[A](columns(0), rs, offset + 1)
    val t2 = Conversion.getValue[B](columns(1), rs, offset + 2)
    val t3 = Conversion.getValue[C](columns(2), rs, offset + 3)
    val t4 = Conversion.getValue[D](columns(3), rs, offset + 4)
    val t5 = Conversion.getValue[E](columns(4), rs, offset + 5)
    val t6 = Conversion.getValue[F](columns(5), rs, offset + 6)
    val t7 = Conversion.getValue[G](columns(6), rs, offset + 7)
    val t8 = Conversion.getValue[H](columns(7), rs, offset + 8)
    val tuple = Tuple8[A, B, C, D, E, F, G, H](t1, t2, t3, t4, t5, t6, t7, t8)
    t(tuple)
  }
}
