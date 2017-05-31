package clunk.mappers

import clunk.Table
import java.sql.ResultSet

class ResultSetMapper2[A <: Table[_], B <: Table[_]](source: Tuple2[A, B]) {

  def map(rs: ResultSet) = {
    var result = List[Tuple2[A#M, B#M]]()

    while (rs.next()) {
      val left = source._1.fromDb(rs)
      val right = source._2.fromDb(rs, source._1.projectionArity)

      result = (left, right) :: result
    }
    result
  }
}
