package clunk

import java.sql.ResultSet

trait TypeTag

object TypeTag {

  case object IntTag extends TypeTag
  case object StrTag extends TypeTag
}

trait Nameable {
  val srcName: String
}

trait Taggable {
  val typeTag: TypeTag
}

trait ColumnLike[A] extends Nameable with Taggable

trait Convertible[A] {
  val arity: Int
  def fromDb(rs: ResultSet, offset: Int): A
}

class Converter2[B, C, A](val t: Tuple2[B, C] => A) extends Convertible[A] {
  val arity = 2
  def fromDb(rs: ResultSet, offset: Int) = {
    val tuple = Tuple2[B, C](
      rs.getObject(offset + 1).asInstanceOf[B],
      rs.getObject(offset + 2).asInstanceOf[C]
    )
    t(tuple)
  }
}

class Converter3[B, C, D, A](val t: Tuple3[B, C, D] => A) extends Convertible[A] {
  val arity = 3
  def fromDb(rs: ResultSet, offset: Int) = {
    val tuple = Tuple3[B, C, D](
      rs.getObject(offset + 1).asInstanceOf[B],
      rs.getObject(offset + 2).asInstanceOf[C],
      rs.getObject(offset + 3).asInstanceOf[D]
    )
    t(tuple)
  }
}

class Converter4[B, C, D, E, A](val t: Tuple4[B, C, D, E] => A) extends Convertible[A] {
  val arity = 4
  def fromDb(rs: ResultSet, offset: Int) = {
    val tuple = Tuple4[B, C, D, E](
      rs.getObject(offset + 1).asInstanceOf[B],
      rs.getObject(offset + 2).asInstanceOf[C],
      rs.getObject(offset + 3).asInstanceOf[D],
      rs.getObject(offset + 4).asInstanceOf[E]
    )
    t(tuple)
  }
}

trait TableLike[A] extends Nameable {
  type M = A
  val conversions: Convertible[A]

  def mapping[B, C](c: Tuple2[Column[B], Column[C]], t: Tuple2[B, C] => A) =
    new Converter2[B, C, A](t)

  def mapping[B, C, D](c: Tuple3[Column[B], Column[C], Column[D]], t: Tuple3[B, C, D] => A) =
    new Converter3[B, C, D, A](t)

  def mapping[B, C, D, E](c: Tuple4[Column[B], Column[C], Column[D], Column[E]], t: Tuple4[B, C, D, E] => A) =
    new Converter4[B, C, D, E, A](t)

  def fromDb(rs: ResultSet, offset: Int = 0) = conversions.fromDb(rs, offset)
  def arity = conversions.arity
}
