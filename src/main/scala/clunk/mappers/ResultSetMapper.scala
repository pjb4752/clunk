package clunk.mappers

import clunk.Table
import java.sql.ResultSet

class ResultSetMapper[A <: Table[_]](source: A) {

  def map(rs: ResultSet) = {
    var result = List[A#M]()

    while (rs.next()) {
      val record = source.fromDb(rs)
      result = record :: result
    }
    result

  }
}
