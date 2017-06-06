package clunk

import java.sql.ResultSet

trait Convertible[A] {
  val arity: Int

  def fromDb(columns: Seq[Column[_, _]], rs: ResultSet, offset: Int): A
}
