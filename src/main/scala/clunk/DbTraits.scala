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
  def fromDb(rs: ResultSet): List[A]
}

class Converter2[B, C, A](val t: Tuple2[B, C] => A) extends Convertible[A] {
  def fromDb(rs: ResultSet) = {
    var result = List[A]()

    while (rs.next()) {
      val tuple = Tuple2[B, C](
        rs.getObject(1).asInstanceOf[B],
        rs.getObject(2).asInstanceOf[C]
      )
      result = t(tuple) :: result
    }
    result
  }
}

class Converter3[B, C, D, A](val t: Tuple3[B, C, D] => A) extends Convertible[A] {
  def fromDb(rs: ResultSet) = {
    var result = List[A]()

    while (rs.next()) {
      val tuple = Tuple3[B, C, D](
        rs.getObject(1).asInstanceOf[B],
        rs.getObject(2).asInstanceOf[C],
        rs.getObject(3).asInstanceOf[D]
      )
      result = t(tuple) :: result
    }
    result
  }
}

class Converter4[B, C, D, E, A](val t: Tuple4[B, C, D, E] => A) extends Convertible[A] {
  def fromDb(rs: ResultSet) = {
    var result = List[A]()

    while (rs.next()) {
      val tuple = Tuple4[B, C, D, E](
        rs.getObject(1).asInstanceOf[B],
        rs.getObject(2).asInstanceOf[C],
        rs.getObject(3).asInstanceOf[D],
        rs.getObject(4).asInstanceOf[E]
      )
      result = t(tuple) :: result
    }
    result
  }
}

trait TableLike[A] extends Nameable {
  val conversions: Convertible[A]

  def mapping[B, C](c: Tuple2[Column[B], Column[C]], t: Tuple2[B, C] => A) =
    new Converter2[B, C, A](t)

  def mapping[B, C, D](c: Tuple3[Column[B], Column[C], Column[D]], t: Tuple3[B, C, D] => A) =
    new Converter3[B, C, D, A](t)

  def mapping[B, C, D, E](c: Tuple4[Column[B], Column[C], Column[D], Column[E]], t: Tuple4[B, C, D, E] => A) =
    new Converter4[B, C, D, E, A](t)

  def fromDb(rs: ResultSet) = conversions.fromDb(rs)
}
