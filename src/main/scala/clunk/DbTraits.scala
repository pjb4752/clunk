package clunk

import clunk.converters._
import java.sql.ResultSet
import scala.collection.mutable.ArrayBuffer

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

  val columns: ArrayBuffer[Column[_, _]]
  val converter: Convertible[Record]

  def projectionMapping[A, B](
      c: Tuple2[Column[A, _], Column[B, _]],
      t: Tuple2[A, B] => Record) =
    new Converter2[A, B, Record](t)

  def projectionMapping[A, B, C](
      c: Tuple3[Column[A, _], Column[B, _], Column[C, _]],
      t: Tuple3[A, B, C] => Record) =
    new Converter3[A, B, C, Record](t)

  def projectionMapping[A, B, C, D](
      c: Tuple4[Column[A, _], Column[B, _], Column[C, _], Column[D, _]],
      t: Tuple4[A, B, C, D] => Record) =
    new Converter4[A, B, C, D, Record](t)

  def projectionMapping[A, B, C, D, E](
      c: Tuple5[Column[A, _], Column[B, _], Column[C, _], Column[D, _], Column[E, _]],
      t: Tuple5[A, B, C, D, E] => Record) =
    new Converter5[A, B, C, D, E, Record](t)

  def projectionMapping[A, B, C, D, E, F](
      c: Tuple6[Column[A, _], Column[B, _], Column[C, _], Column[D, _], Column[E, _], Column[F, _]],
      t: Tuple6[A, B, C, D, E, F] => Record) =
    new Converter6[A, B, C, D, E, F, Record](t)

  def projectionMapping[A, B, C, D, E, F, G](
      c: Tuple7[Column[A, _], Column[B, _], Column[C, _], Column[D, _], Column[E, _], Column[F, _], Column[G, _]],
      t: Tuple7[A, B, C, D, E, F, G] => Record) =
    new Converter7[A, B, C, D, E, F, G, Record](t)

  def projectionMapping[A, B, C, D, E, F, G, H](
      c: Tuple8[Column[A, _], Column[B, _], Column[C, _], Column[D, _], Column[E, _], Column[F, _], Column[G, _], Column[H, _]],
      t: Tuple8[A, B, C, D, E, F, G, H] => Record) =
    new Converter8[A, B, C, D, E, F, G, H, Record](t)

  def projectionMapping[A, B, C, D, E, F, G, H, I](
      c: Tuple9[Column[A, _], Column[B, _], Column[C, _], Column[D, _], Column[E, _], Column[F, _], Column[G, _], Column[H, _], Column[I, _]],
      t: Tuple9[A, B, C, D, E, F, G, H, I] => Record) =
    new Converter9[A, B, C, D, E, F, G, H, I, Record](t)

  def projectionArity = converter.arity

  def fromDb(rs: ResultSet, offset: Int = 0): Record =
    converter.fromDb(columns.toSeq, rs, offset)
}
