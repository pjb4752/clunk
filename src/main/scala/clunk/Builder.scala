package clunk

sealed trait ColumnBuilder[A] {
  def build(name: String): Column[A]
}

object Builder {

  implicit object IntColumnBuilder extends ColumnBuilder[Int] {
    def build(name: String) = new Column[Int](TypeTag.IntTag, name)
  }

  implicit object StringColumnBuilder extends ColumnBuilder[String] {
    def build(name: String) = new Column[String](TypeTag.StrTag, name)
  }
}
