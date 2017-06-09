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
        for ((col, i) <- columns.view.zipWithIndex) {
          if (!col.isAutoGen) {
            stmtBindParam(col, tuple.productElement(i), current)
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

  def update(sql: String, fieldBindings: Seq[Tuple2[Column[_, _], Any]],
      keyBindings: Seq[Tuple2[Column[_, _], Any]]) = {
    val statement = underlying.prepareStatement(sql)
    var rowsAffected = 0

    try {
      val stmtBindParam = (bindParam _)(statement)
      var current = 1

      for ((col, value) <- fieldBindings) {
        stmtBindParam(col, value, current)
        current += 1
      }
      for ((col, value) <- keyBindings) {
        stmtBindParam(col, value, current)
        current += 1
      }
      rowsAffected = statement.executeUpdate()
    } finally {
      statement.close()
    }
    rowsAffected
  }

  private def bindParam(stmt: PreparedStatement)
      (col: Column[_, _], value: Any, i: Int) = {
    val sqlType = col.typeTag match {
      case TypeTag.IntTag => java.sql.Types.INTEGER
      case TypeTag.StrTag => java.sql.Types.VARCHAR
    }

    if (col.isNullable) {
      val maybeValue = value.asInstanceOf[Option[Any]]
      stmt.setObject(i, maybeValue.getOrElse(null), sqlType)
    } else {
      stmt.setObject(i, value, sqlType)
    }
  }
}
