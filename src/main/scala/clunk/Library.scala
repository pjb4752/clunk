package clunk

import clunk.Builder._
import clunk.jdbc.Database

object Library extends App {
  case class User(id: Int, name: String, email: String, addressId: Int)
  case class Role(id: Int, name: String, userId: Int)
  case class Address(id: Int, street1: String, street2: Option[String],
    aptNum: Option[Int], city: String, state: String, country: String)
  case class Order(id: Int, userId: Int)
  case class Item(id: Int, price: Int, quantity: Int, orderId: Int)

  object UserTable extends Table("users") {
    type Record = User

    val id = column[Int]("id")
    val name = column[String]("name")
    val email = column[String]("email")
    val addressId = column[Int]("address_id")

    val roles = oneToMany(RoleTable, id, RoleTable.userId)
    val address = oneToOne(AddressTable, addressId, AddressTable.id)
    val orders = oneToMany(OrderTable, id, OrderTable.userId)

    val converter = projectionMapping(
      (
        id,
        name,
        email,
        addressId),
      User.tupled, User.unapply _)
  }

  object RoleTable extends Table("roles") {
    type Record = Role

    val id = column[Int]("id")
    val name = column[String]("name")
    val userId = column[Int]("user_id")

    val user = manyToOne(UserTable, UserTable.id, userId)

    val converter = projectionMapping(
      (
        id,
        name,
        userId),
      Role.tupled, Role.unapply _)
  }

  object AddressTable extends Table("addresses") {
    type Record = Address

    val id = column[Int]("id")
    val street1 = column[String]("street1")
    val street2 = column[Option[String]]("street2")
    val aptNum = column[Option[Int]]("apt_num")
    val city = column[String]("city")
    val state = column[String]("state")
    val country = column[String]("country")

    val user = oneToOne(UserTable, UserTable.addressId, id)

    val converter = projectionMapping(
      (
        id,
        street1,
        street2,
        aptNum,
        city,
        state,
        country),
      Address.tupled, Address.unapply _)
  }

  object OrderTable extends Table("orders") {
    type Record = Order

    val id = column[Int]("id")
    val userId = column[Int]("user_id")

    val user = manyToOne(UserTable, UserTable.id, userId)
    val items = oneToMany(ItemTable, id, ItemTable.orderId)

    val converter = projectionMapping(
      (
        id,
        userId),
      Order.tupled, Order.unapply _)
  }

  object ItemTable extends Table("items") {
    type Record = Item

    val id = column[Int]("id")
    val price = column[Int]("price")
    val quantity = column[Int]("quantity")
    val orderId = column[Int]("order_id")

    val order = manyToOne(OrderTable, OrderTable.id, orderId)

    val converter = projectionMapping(
      (
        id,
        price,
        quantity,
        orderId),
      Item.tupled, Item.unapply _)
  }

  //val item = Item(3, 1000, 9, 1)
  //val insert = Insert(ItemTable)

  insert.execute(item)

  val query = Query(UserTable).
    innerJoin(_.roles).
    innerJoin({ case (u, r) => u.address }).
    innerJoin({ case (u, r, a) => u.orders }).
    innerJoin({ case (u, r, a, o) => o.items }).
    where({ case (u, r, a, o, i) => u.name.isEqualTo("Pat") }).
    where({ case (u, r, a, o, i) => a.state.isEqualTo("PA") })

  println(query.toSql)
  val result: List[(User, Role, Address, Order, Item)] = query.result
  println(result)
}
