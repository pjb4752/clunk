package clunk

import clunk.Builder._
import clunk.{ ColumnFlag => Flags }
import clunk.jdbc.Database

object Library extends App {
  case class User(id: Option[Int], name: String, email: String,
      loginCount: Int, addressId: Int)
  case class Role(id: Option[Int], name: String, userId: Int)
  case class Address(id: Option[Int], street1: String, street2: Option[String],
      aptNum: Option[Int], city: String, state: String, country: String)
  case class Order(id: Option[Int], userId: Int)
  case class Item(id: Option[Int], price: Int, quantity: Int, orderId: Int)

  object UserTable extends Table("users") {
    type Record = User

    val id = column[Option[Int]]("id", Flags.AutoGen)
    val name = column[String]("name")
    val email = column[String]("email")
    val loginCount = column[Int]("login_count")
    val addressId = column[Int]("address_id")

    val roles = oneToMany(RoleTable, id, RoleTable.userId)
    val address = oneToOne(AddressTable, addressId, AddressTable.id)
    val orders = oneToMany(OrderTable, id, OrderTable.userId)

    val primaryKeys = primaryKey(id)
    val converter = projectionMapping(
      (
        id,
        name,
        email,
        loginCount,
        addressId))(User.tupled)(User.unapply)
  }

  object RoleTable extends Table("roles") {
    type Record = Role

    val id = column[Option[Int]]("id", Flags.AutoGen)
    val name = column[String]("name")
    val userId = column[Int]("user_id")

    val user = manyToOne(UserTable, UserTable.id, userId)

    val primaryKeys = primaryKey(id)
    val converter = projectionMapping(
      (
        id,
        name,
        userId))(Role.tupled)(Role.unapply)
  }

  object AddressTable extends Table("addresses") {
    type Record = Address

    val id = column[Option[Int]]("id", Flags.AutoGen)
    val street1 = column[String]("street1")
    val street2 = column[Option[String]]("street2")
    val aptNum = column[Option[Int]]("apt_num")
    val city = column[String]("city")
    val state = column[String]("state")
    val country = column[String]("country")

    val user = oneToOne(UserTable, UserTable.addressId, id)

    val primaryKeys = primaryKey(id)
    val converter = projectionMapping(
      (
        id,
        street1,
        street2,
        aptNum,
        city,
        state,
        country))(Address.tupled)(Address.unapply)
  }

  object OrderTable extends Table("orders") {
    type Record = Order

    val id = column[Option[Int]]("id", Flags.AutoGen)
    val userId = column[Int]("user_id")

    val user = manyToOne(UserTable, UserTable.id, userId)
    val items = oneToMany(ItemTable, id, ItemTable.orderId)

    val primaryKeys = primaryKey(id)
    val converter = projectionMapping(
      (
        id,
        userId))(Order.tupled)(Order.unapply)
  }

  object ItemTable extends Table("items") {
    type Record = Item

    val id = column[Option[Int]]("id", Flags.AutoGen)
    val price = column[Int]("price")
    val quantity = column[Int]("quantity")
    val orderId = column[Int]("order_id")

    val order = manyToOne(OrderTable, OrderTable.id, orderId)

    val primaryKeys = primaryKey(id)
    val converter = projectionMapping(
      (
        id,
        price,
        quantity,
        orderId))(Item.tupled)(Item.unapply)
  }

  val userQuery = Query(UserTable)
  val roleQuery = Query(RoleTable)
  val addressQuery = Query(AddressTable)
  val orderQuery = Query(OrderTable)
  val itemQuery = Query(ItemTable)

  val order = orderQuery.result.headOption

  val itemsInserted = order.map({ o =>
    val item = Item(None, 1000, 9, 1)
    itemQuery.insert(item)
  }).getOrElse(0)

  println("# of items inserted = " + itemsInserted)

  val user = userQuery.where(_.name.isEqualTo("Pat")).result.headOption

  val usersUpdated = user.map({ u =>
    val changedUser = u.copy(loginCount = u.loginCount+1)
    userQuery.update(changedUser)
  }).getOrElse(0)

  println("# of users updated = " + usersUpdated)

  val bigQuery = userQuery.
    innerJoin(_.roles).
    innerJoin({ case (u, r) => u.address }).
    innerJoin({ case (u, r, a) => u.orders }).
    innerJoin({ case (u, r, a, o) => o.items }).
    where({ case (u, r, a, o, i) => u.name.isEqualTo("Pat") }).
    where({ case (u, r, a, o, i) => a.state.isEqualTo("PA") })

  println(bigQuery.toSql)
  val result: List[(User, Role, Address, Order, Item)] = bigQuery.result
  println(result)
}
