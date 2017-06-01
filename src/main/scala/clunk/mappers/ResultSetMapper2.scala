package clunk.mappers

import clunk.Table
import java.sql.ResultSet

class ResultSetMapper2[T1 <: Table, T2 <: Table](source: Tuple2[T1, T2]) {

  def map(rs: ResultSet) = {
    var result = List[Tuple2[T1#Record, T2#Record]]()

    while (rs.next()) {
      val left = source._1.fromDb(rs)
      val right = source._2.fromDb(rs, source._1.projectionArity)

      result = (left, right) :: result
    }
    result
  }
}
