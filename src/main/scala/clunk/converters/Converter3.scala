package clunk.converters

import clunk.{Column, Convertible}
import java.sql.ResultSet

class Converter3[A, B, C, Record](
    t: Tuple3[A, B, C] => Record,
    r: Record => Option[Tuple3[A, B, C]])
    extends Convertible[Record] {

  type Unwrapped = Tuple3[A, B, C]
  type Unapplied = Option[Unwrapped]
  val arity = 3

  def fromDb(columns: Seq[Column[_, _]], rs: ResultSet, offset: Int) = {
    val t1 = Conversion.getValue[A](columns(0), rs, offset + 1)
    val t2 = Conversion.getValue[B](columns(1), rs, offset + 2)
    val t3 = Conversion.getValue[C](columns(2), rs, offset + 3)
    val tuple = (t1, t2, t3)
    t(tuple)
  }

  def toDb(record: Record) = r(record)
}
