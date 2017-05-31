package clunk

import clunk.Builder._
import clunk.jdbc.Database

object Library extends App {
  case class User(id: Int, name: String, addressId: Int)
  case class Role(id: Int, name: String, userId: Int)
  case class Address(id: Int, street: String, city: String, state: String)

  object UserTable extends Table[User]("users") {
    val id = column[Int]("id")
    val name = column[String]("name")
    val addressId = column[Int]("address_id")

    val roles = oneToMany(RoleTable, id, RoleTable.userId)
    val address = oneToOne(AddressTable, addressId, AddressTable.id)

    val converter = projectionMapping(
      (
        id,
        name,
        addressId),
      User.tupled)
  }

  object RoleTable extends Table[Role]("roles") {
    val id = column[Int]("id")
    val name = column[String]("name")
    val userId = column[Int]("user_id")

    val user = manyToOne(UserTable, UserTable.id, userId)

    val converter = projectionMapping(
      (
        id,
        name,
        userId),
      Role.tupled)
  }

  object AddressTable extends Table[Address]("addresses") {
    val id = column[Int]("id")
    val street = column[String]("street")
    val city = column[String]("city")
    val state = column[String]("state")

    val user = oneToOne(UserTable, UserTable.addressId, id)

    val converter = projectionMapping(
      (
        id,
        street,
        city,
        state),
      Address.tupled)
  }

  Database.init()

  val query = Query(UserTable).
    innerJoin(_.roles).
    //innerJoin({ case (u, r) => u.address }).
    filter({ case (u, r) => u.name.isEqualTo("Pat") }).
    filter({ case (u, r) => r.name.isEqualTo("admin") })
    //filter({ case (u, r, a) => a.state.isEqualTo("PA") })

  println(query.toSql)
  println(query.result)
}

