package clunk.jdbc

import clunk.Ast.Node.Comparison
import clunk.TypeTag
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
        stmtBindParam(f, i)
      }

      val resultSet = statement.executeQuery()
      fn(resultSet)
    } finally {
      statement.close()
    }
  }

  def bindParam(stmt: PreparedStatement)(cmp: Comparison[_, _], i: Int) = {
    val sqlType = cmp.column.typeTag match {
      case TypeTag.IntTag => java.sql.Types.INTEGER
      case TypeTag.StrTag => java.sql.Types.VARCHAR
    }

    stmt.setObject(i + 1, cmp.value, sqlType)
  }
}
