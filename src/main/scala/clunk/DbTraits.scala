package clunk

import clunk.converters._
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

trait TableLike[A] extends Nameable {
  type M = A

  val converter: Convertible[A]

  def projectionMapping[B, C](
      c: Tuple2[Column[B], Column[C]],
      t: Tuple2[B, C] => A) =
    new Converter2[B, C, A](t)

  def projectionMapping[B, C, D](
      c: Tuple3[Column[B], Column[C], Column[D]],
      t: Tuple3[B, C, D] => A) =
    new Converter3[B, C, D, A](t)

  def projectionMapping[B, C, D, E](
      c: Tuple4[Column[B], Column[C], Column[D], Column[E]],
      t: Tuple4[B, C, D, E] => A) =
    new Converter4[B, C, D, E, A](t)

  def projectionArity = converter.arity

  def fromDb(rs: ResultSet, offset: Int = 0) = converter.fromDb(rs, offset)
}
