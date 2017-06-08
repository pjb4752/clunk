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
      var current = 1

      try {
        if (!columns(0).isAutoGen) {
          stmtBindParam(columns(0), tuple._1, current)
          current += 1
        }
        if (!columns(current).isAutoGen) {
          stmtBindParam(columns(1), tuple._2, current)
        }
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
      var current = 0

      try {
        if (!columns(0).isAutoGen) {
          stmtBindParam(columns(0), tuple._1, current)
          current += 1
        }
        if (!columns(1).isAutoGen) {
          stmtBindParam(columns(1), tuple._2, current)
          current += 1
        }
        if (!columns(2).isAutoGen) {
          stmtBindParam(columns(2), tuple._3, current)
        }
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
      var current = 1

      try {
        if (!columns(0).isAutoGen) {
          stmtBindParam(columns(0), tuple._1, current)
          current += 1
        }
        if (!columns(1).isAutoGen) {
          stmtBindParam(columns(1), tuple._2, current)
          current += 1
        }
        if (!columns(2).isAutoGen) {
          stmtBindParam(columns(2), tuple._3, current)
          current += 1
        }
        if (!columns(3).isAutoGen) {
          stmtBindParam(columns(3), tuple._4, current)
        }
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
