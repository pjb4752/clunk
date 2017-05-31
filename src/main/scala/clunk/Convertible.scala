package clunk

import java.sql.ResultSet

trait Convertible[A] {
  val arity: Int

  def fromDb(rs: ResultSet, offset: Int): A
}
