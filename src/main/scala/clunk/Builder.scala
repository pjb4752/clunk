package clunk

sealed trait ColumnBuilder[A] {
  def build(table: Table, name: String): Column[A]
}

object Builder {

  implicit object IntColumnBuilder extends ColumnBuilder[Int] {
    def build(table: Table, name: String) =
      new Column[Int](TypeTag.IntTag, table, name)
  }

  implicit object StringColumnBuilder extends ColumnBuilder[String] {
    def build(table: Table, name: String) =
      new Column[String](TypeTag.StrTag, table, name)
  }
}
