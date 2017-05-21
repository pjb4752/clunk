package clunk

import clunk.Builder._

object Library extends App {
  case class User(id: Int, name: String, roleId: Int)
  case class Role(id: Int, name: String)

  object UserTable extends Table[User]("users") {
    val id = column[Int]("id")
    val name = column[String]("name")
    val roleId = column[Int]("role_id")

    val roles = oneToMany(RoleTable, roleId, RoleTable.id)
  }

  object RoleTable extends Table[Role]("roles") {
    val id = column[Int]("id")
    val name = column[String]("name")

    val user = manyToOne(UserTable, UserTable.roleId, id)
  }

  val users = UserTable
  val roles = RoleTable

  // these do not compile
  // val sql = Query(UserTable).filter(_.fooBar.isEqualTo(1)).toSql
  // val sql = Query(UserTable).filter(_.id.isEqualTo("something")).toSql
  val sql = Query(UserTable).
    filter(_.id.isEqualTo(1)).
    filter(_.name.isEqualTo("Pat")).
    toSql

  // SELECT `id`, `name`, `role_id` FROM `users` WHERE `id` = 1
  println(sql)

}
