package clunk.mappers

import clunk.Table
import java.sql.ResultSet

class ResultSetMapper3[T1 <: Table, T2 <: Table, T3 <: Table]
    (source: Tuple3[T1, T2, T3]) {

  def map(rs: ResultSet) = {
    var result = List[Tuple3[T1#Record, T2#Record, T3#Record]]()

    while (rs.next()) {
      var offset = 0

      val record1 = source._1.fromDb(rs)
      offset += source._1.projectionArity
      val record2 = source._2.fromDb(rs, offset)
      offset += source._2.projectionArity
      val record3 = source._3.fromDb(rs, offset)

      result = (record1, record2, record3) :: result
    }
    result
  }
}
