package clunk.converters

import clunk.{Column, Convertible}
import java.sql.ResultSet

class Converter2[A, B, Record](
    t: Tuple2[A, B] => Record,
    r: Record => Option[Tuple2[A, B]])
    extends Convertible[Record] {

  type Unwrapped = Tuple2[A, B]
  type Unapplied = Option[Unwrapped]
  val arity = 2

  def fromDb(columns: Seq[Column[_, _]], rs: ResultSet, offset: Int) = {
    val t1 = Conversion.getValue[A](columns(0), rs, offset + 1)
    val t2 = Conversion.getValue[B](columns(1), rs, offset + 2)
    val tuple = (t1, t2)

    t(tuple)
  }

  def toDb(record: Record) = r(record)
}

