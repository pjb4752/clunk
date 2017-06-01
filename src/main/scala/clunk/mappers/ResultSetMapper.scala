package clunk.mappers

import clunk.Table
import java.sql.ResultSet

class ResultSetMapper[T1 <: Table](source: T1) {
  def map(rs: ResultSet) = {
    var result = List[T1#Record]()

    while (rs.next()) {
      val record = source.fromDb(rs)
      result = record :: result
    }
    result

  }
}
