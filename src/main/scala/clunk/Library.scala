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

  val userSql = Query(users).
    innerJoin(_.roles).
    filter({ case (u, r) => u.id.isEqualTo(1) }).
    filter({ case (u, r) => u.name.isEqualTo("Pat") }).
    toSql

  println(userSql)

  val roleSql = Query(roles).
    innerJoin(_.user).
    filter({ case (r, u) => r.name.isEqualTo("admin") }).
    toSql

  println(roleSql)
}

