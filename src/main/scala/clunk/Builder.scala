package clunk

import scala.reflect.ClassTag

sealed trait ColumnBuilder[A] {
  type B

  def build(table: Table, name: String): Column[A, B]
}

object Builder {

  implicit object NotNullIntBuilder extends ColumnBuilder[Int] {
    type B = Int
    def build(table: Table, name: String) =
      new Column[Int, B](TypeTag.IntTag, table, name, isNullable = false)
  }

  implicit object NotNullStringBuilder extends ColumnBuilder[String] {
    type B = String
    def build(table: Table, name: String) =
      new Column[String, B](TypeTag.StrTag, table, name, isNullable = false)
  }

  implicit object NullableIntBuilder extends ColumnBuilder[Option[Int]] {
    type B = Int
    def build(table: Table, name: String) =
      new Column[Option[Int], B](TypeTag.IntTag, table, name, isNullable = true)
  }

  implicit object NullableStringBuilder extends ColumnBuilder[Option[String]] {
    type B = String
    def build(table: Table, name: String) =
      new Column[Option[String], B](TypeTag.StrTag, table, name, isNullable = true)
  }
}
