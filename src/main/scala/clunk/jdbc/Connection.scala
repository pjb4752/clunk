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

  def insert(sql: String, columns: Seq[Column[_, _]],
      values: Option[Product]) = {
    values.map({ tuple =>
      val statement = underlying.prepareStatement(sql)
      var rowsAffected = 0

      try {
        val stmtBindParam = (bindParam _)(statement)
        var current = 1
        for ((f, i) <- columns.view.zipWithIndex) {
          if (!columns(i).isAutoGen) {
            stmtBindParam(columns(i), tuple.productElement(i), current)
            current += 1
          }
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
