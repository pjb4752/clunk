package clunk

import scala.reflect.ClassTag

sealed trait ColumnBuilder[A] {
  type B

  def build(table: Table, name: String, flags: Seq[ColumnFlag]): Column[A, B]
}

object Builder {

  implicit object NotNullIntBuilder extends ColumnBuilder[Int] {
    type B = Int
    def build(table: Table, name: String, flags: Seq[ColumnFlag]) = {
      val autoGenFlag = isAutoGen(flags)
      new Column[Int, B](TypeTag.IntTag, table, name, isNullable = false,
          isAutoGen = autoGenFlag)
    }
  }

  implicit object NotNullStringBuilder extends ColumnBuilder[String] {
    type B = String
    def build(table: Table, name: String, flags: Seq[ColumnFlag]) = {
      val autoGenFlag = isAutoGen(flags)
      new Column[String, B](TypeTag.StrTag, table, name, isNullable = false,
          isAutoGen = autoGenFlag)
    }
  }

  implicit object NullableIntBuilder extends ColumnBuilder[Option[Int]] {
    type B = Int
    def build(table: Table, name: String, flags: Seq[ColumnFlag]) = {
      val autoGenFlag = isAutoGen(flags)
      new Column[Option[Int], B](TypeTag.IntTag, table, name,
          isNullable = true, isAutoGen = autoGenFlag)
    }
  }

  implicit object NullableStringBuilder extends ColumnBuilder[Option[String]] {
    type B = String
    def build(table: Table, name: String, flags: Seq[ColumnFlag]) = {
      val autoGenFlag = isAutoGen(flags)
      new Column[Option[String], B](TypeTag.StrTag, table, name,
          isNullable = true, isAutoGen = autoGenFlag)
    }
  }

  private def isAutoGen(seq: Seq[ColumnFlag]) =
    seq.contains(ColumnFlag.AutoGen)
}
