package clunk.jdbc

import clunk.Ast.Node.Comparison
import clunk.{Column, TypeTag}
import clunk.TypeTag._
import java.sql.{Connection => JavaConn}
import java.sql.{PreparedStatement, ResultSet, SQLException}

class Connection(underlying: JavaConn) {

  def query[A](sql: String, filters: Seq[Comparison[_, _]],
      fn: ResultSet => A) = {

    val statement = underlying.prepareStatement(sql)
    try {
      val stmtBindParam = (bindParam _)(statement)

      for ((f, i) <- filters.view.zipWithIndex) {
        stmtBindParam(f.column, f.value, i + 1)
      }

      val resultSet = statement.executeQuery()
      fn(resultSet)
    } finally {
      statement.close()
    }
  }

  def insert2[A, B](sql: String, columns: Seq[Column[_, _]],
      values: Option[Tuple2[A, B]]) = {
    values.map({ tuple =>
      val statement = underlying.prepareStatement(sql)
      val stmtBindParam = (bindParam _)(statement)
      var rowsAffected = 0

      try {
        stmtBindParam(columns(0), tuple._1, 1)
        stmtBindParam(columns(1), tuple._2, 2)
        rowsAffected = statement.executeUpdate()
      } finally {
        statement.close()
      }
      rowsAffected
    }).getOrElse(0)
  }

  def insert3[A, B, C](sql: String, columns: Seq[Column[_, _]],
      values: Option[Tuple3[A, B, C]]) = {
    values.map({ tuple =>
      val statement = underlying.prepareStatement(sql)
      val stmtBindParam = (bindParam _)(statement)
      var rowsAffected = 0

      try {
        stmtBindParam(columns(0), tuple._1, 1)
        stmtBindParam(columns(1), tuple._2, 2)
        stmtBindParam(columns(2), tuple._3, 3)
        rowsAffected = statement.executeUpdate()
      } finally {
        statement.close()
      }
      rowsAffected
    }).getOrElse(0)
  }

  def insert4[A, B, C, D](sql: String, columns: Seq[Column[_, _]],
      values: Option[Tuple4[A, B, C, D]]) = {
    values.map({ tuple =>
      val statement = underlying.prepareStatement(sql)
      val stmtBindParam = (bindParam _)(statement)
      var rowsAffected = 0

      try {
        stmtBindParam(columns(0), tuple._1, 1)
        stmtBindParam(columns(1), tuple._2, 2)
        stmtBindParam(columns(2), tuple._3, 3)
        stmtBindParam(columns(3), tuple._4, 4)
        rowsAffected = statement.executeUpdate()
      } finally {
        statement.close()
      }
      rowsAffected
    }).getOrElse(0)
  }

  private def bindParam(stmt: PreparedStatement)
      (col: Column[_, _], value: Any, i: Int) = {
    val sqlType = col.typeTag match {
      case TypeTag.IntTag => java.sql.Types.INTEGER
      case TypeTag.StrTag => java.sql.Types.VARCHAR
    }

    stmt.setObject(i, value, sqlType)
  }
}
