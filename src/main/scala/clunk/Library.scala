package clunk

import clunk.Builder._

object Library extends App {
  case class User(id: Int, name: String, roleId: Int)
  case class Role(id: Int, name: String)
  case class Address(id: Int, street: String, city: String, state: String)

  object UserTable extends Table[User]("users") {
    val id = column[Int]("id")
    val name = column[String]("name")
    val addressId = column[Int]("address_id")

    val roles = oneToMany(RoleTable, id, RoleTable.userId)
    val address = oneToOne(AddressTable, addressId, AddressTable.id)
  }

  object RoleTable extends Table[Role]("roles") {
    val id = column[Int]("id")
    val name = column[String]("name")
    val userId = column[Int]("user_id")
    val addressId = column[Int]("address_id")

    val user = manyToOne(UserTable, UserTable.id, userId)
  }

  object AddressTable extends Table[Address]("addresses") {
    val id = column[Int]("id")
    val street = column[String]("street")
    val city = column[String]("city")
    val state = column[String]("state")

    val user = oneToOne(UserTable, UserTable.addressId, id)
  }

  val userSql = Query(UserTable).
    innerJoin(_.roles).
    innerJoin({ case (u, r) => u.address }).
    filter({ case (u, r, a) => u.name.isEqualTo("Pat") }).
    filter({ case (u, r, a) => r.name.isEqualTo("admin") }).
    filter({ case (u, r, a) => a.state.isEqualTo("Ohio") }).
    toSql

  println(userSql)
}

