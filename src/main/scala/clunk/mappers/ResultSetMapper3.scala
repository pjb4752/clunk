package clunk.mappers

import clunk.Table
import java.sql.ResultSet

class ResultSetMapper3[A <: Table[_], B <: Table[_], C <: Table[_]]
    (source: Tuple3[A, B, C]) {

  def map(rs: ResultSet) = {
    var result = List[Tuple3[A#M, B#M, C#M]]()

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
