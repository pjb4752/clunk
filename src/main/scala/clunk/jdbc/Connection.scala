package clunk.jdbc

import java.sql.{Connection => JavaConn}

class Connection(underlying: JavaConn) {

  def execute(sql: String) = underlying.createStatement.executeQuery(sql)
}
