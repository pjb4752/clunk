package clunk.jdbc

import java.sql.DriverManager

object Database {
  val url = "jdbc:mysql://localhost/clunk"
  val user = "root"
  val password = ""
  val options = Map(
    ("useSSL", false),
    ("characterEncoding", "UTF-8"),
    ("serverTimezone", "UTC"))

  def init(): Unit = {
    Class.forName("com.mysql.cj.jdbc.Driver")
  }

  def connection() = {
    val underlying = DriverManager.getConnection(uri, user, password)
    new Connection(underlying)
  }

  lazy val uri = url + options.map({ case (k, v) => s"${k}=${v}" }).
    mkString("?", "&", "").toString
}
