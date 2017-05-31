package clunk.jdbc

import java.sql.DriverManager

object Database {
  val base = "jdbc:mysql://localhost/clunk"
  val user = "root"
  val password = ""
  val options = Map(
    ("useSSL", false),
    ("characterEncoding", "UTF-8"),
    ("serverTimezone", "UTC"))

  val _ = Class.forName("com.mysql.cj.jdbc.Driver")

  def connection() = {
    val underlying = DriverManager.getConnection(url, user, password)
    new Connection(underlying)
  }

  lazy val url = base + options.map({ case (k, v) => s"${k}=${v}" }).
    mkString("?", "&", "").toString
}
