package clunk.converters

import clunk.{Column, Convertible}
import java.sql.ResultSet

class Converter2[A, B, Record](val t: Tuple2[A, B] => Record)
    extends Convertible[Record] {

  val arity = 2

  def fromDb(columns: Seq[Column[_, _]], rs: ResultSet, offset: Int) = {
    val t1 = Conversion.getValue[A](columns(0), rs, offset + 1)
    val t2 = Conversion.getValue[B](columns(1), rs, offset + 2)
    val tuple = Tuple2[A, B](t1, t2)

    t(tuple)
  }
}

