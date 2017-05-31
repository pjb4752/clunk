package clunk

import java.sql.ResultSet

class DbConverter[A <: Table[_], B <: Table[_]](val rs: ResultSet, val source: Tuple2[A, B]) {
  def convert = {
    var result = List[Tuple2[A#M, B#M]]()

    while (rs.next()) {
      val left = source._1.fromDb(rs)
      val right = source._2.fromDb(rs, source._1.arity)

      result = (left, right) :: result
    }

    result
  }
}
