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

trait TableLike extends Nameable {
  type Record

  val converter: Convertible[Record]

  def projectionMapping[A, B](
      c: Tuple2[Column[A], Column[B]],
      t: Tuple2[A, B] => Record) =
    new Converter2[A, B, Record](t)

  def projectionMapping[A, B, C](
      c: Tuple3[Column[A], Column[B], Column[C]],
      t: Tuple3[A, B, C] => Record) =
    new Converter3[A, B, C, Record](t)

  def projectionMapping[A, B, C, D](
      c: Tuple4[Column[A], Column[B], Column[C], Column[D]],
      t: Tuple4[A, B, C, D] => Record) =
    new Converter4[A, B, C, D, Record](t)

  def projectionMapping[A, B, C, D, E](
      c: Tuple5[Column[A], Column[B], Column[C], Column[D], Column[E]],
      t: Tuple5[A, B, C, D, E] => Record) =
    new Converter5[A, B, C, D, E, Record](t)

  def projectionMapping[A, B, C, D, E, F](
      c: Tuple6[Column[A], Column[B], Column[C], Column[D], Column[E], Column[F]],
      t: Tuple6[A, B, C, D, E, F] => Record) =
    new Converter6[A, B, C, D, E, F, Record](t)

  def projectionMapping[A, B, C, D, E, F, G](
      c: Tuple7[Column[A], Column[B], Column[C], Column[D], Column[E], Column[F], Column[G]],
      t: Tuple7[A, B, C, D, E, F, G] => Record) =
    new Converter7[A, B, C, D, E, F, G, Record](t)

  def projectionMapping[A, B, C, D, E, F, G, H](
      c: Tuple8[Column[A], Column[B], Column[C], Column[D], Column[E], Column[F], Column[G], Column[H]],
      t: Tuple8[A, B, C, D, E, F, G, H] => Record) =
    new Converter8[A, B, C, D, E, F, G, H, Record](t)

  def projectionMapping[A, B, C, D, E, F, G, H, I](
      c: Tuple9[Column[A], Column[B], Column[C], Column[D], Column[E], Column[F], Column[G], Column[H], Column[I]],
      t: Tuple9[A, B, C, D, E, F, G, H, I] => Record) =
    new Converter9[A, B, C, D, E, F, G, H, I, Record](t)

  def projectionArity = converter.arity

  def fromDb(rs: ResultSet, offset: Int = 0): Record =
    converter.fromDb(rs, offset)
}
