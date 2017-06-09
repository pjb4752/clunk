package clunk

import java.sql.ResultSet

trait Convertible[Record] {
  type Unapplied
  val arity: Int

  def fromDb(columns: Seq[Column[_, _]], rs: ResultSet, offset: Int): Record
  def toDb(record: Record): Unapplied
}
