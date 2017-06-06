package clunk

import java.sql.ResultSet

object Mapping {

  def map[T1 <: Table](source: T1, rs: ResultSet) = {
    var result = List[T1#Record]()

    while (rs.next()) {
      val record = source.fromDb(rs)
      result = record :: result
    }
    result.reverse
  }

  def map2[T1 <: Table, T2 <: Table](source: Tuple2[T1, T2], rs: ResultSet) = {
    var result = List[Tuple2[T1#Record, T2#Record]]()
    val (table1, table2) = source

    while (rs.next()) {
      var offset = 0

      val r1 = table1.fromDb(rs, offset)
      offset += table1.projectionArity
      val r2 = table2.fromDb(rs, offset)

      result = (r1, r2) :: result
    }
    result.reverse
  }

  def map3[T1 <: Table, T2 <: Table, T3 <: Table]
      (source: Tuple3[T1, T2, T3], rs: ResultSet) = {
    var result = List[Tuple3[T1#Record, T2#Record, T3#Record]]()
    val (table1, table2, table3) = source

    while (rs.next()) {
      var offset = 0

      val r1 = table1.fromDb(rs, offset)
      offset += table1.projectionArity
      val r2 = table2.fromDb(rs, offset)
      offset += table2.projectionArity
      val r3 = table3.fromDb(rs, offset)

      result = (r1, r2, r3) :: result
    }
    result.reverse
  }

  def map4[T1 <: Table, T2 <: Table, T3 <: Table, T4 <: Table]
      (source: Tuple4[T1, T2, T3, T4], rs: ResultSet) = {
    var result = List[Tuple4[T1#Record, T2#Record, T3#Record, T4#Record]]()
    val (table1, table2, table3, table4) = source

    while (rs.next()) {
      var offset = 0

      val r1 = table1.fromDb(rs, offset)
      offset += table1.projectionArity
      val r2 = table2.fromDb(rs, offset)
      offset += table2.projectionArity
      val r3 = table3.fromDb(rs, offset)
      offset += table3.projectionArity
      val r4 = table4.fromDb(rs, offset)

      result = (r1, r2, r3, r4) :: result
    }
    result.reverse
  }

  def map5[T1 <: Table, T2 <: Table, T3 <: Table, T4 <: Table, T5 <: Table]
      (source: Tuple5[T1, T2, T3, T4, T5], rs: ResultSet) = {
    var result = List[Tuple5[T1#Record, T2#Record, T3#Record, T4#Record, T5#Record]]()
    val (table1, table2, table3, table4, table5) = source

    while (rs.next()) {
      var offset = 0

      val r1 = table1.fromDb(rs, offset)
      offset += table1.projectionArity
      val r2 = table2.fromDb(rs, offset)
      offset += table2.projectionArity
      val r3 = table3.fromDb(rs, offset)
      offset += table3.projectionArity
      val r4 = table4.fromDb(rs, offset)
      offset += table4.projectionArity
      val r5 = table5.fromDb(rs, offset)

      result = (r1, r2, r3, r4, r5) :: result
    }
    result.reverse
  }
}
