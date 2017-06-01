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

  def projectionMapping[B, C](
      c: Tuple2[Column[B], Column[C]],
      t: Tuple2[B, C] => Record) =
    new Converter2[B, C, Record](t)

  def projectionMapping[B, C, D](
      c: Tuple3[Column[B], Column[C], Column[D]],
      t: Tuple3[B, C, D] => Record) =
    new Converter3[B, C, D, Record](t)

  def projectionMapping[B, C, D, E](
      c: Tuple4[Column[B], Column[C], Column[D], Column[E]],
      t: Tuple4[B, C, D, E] => Record) =
    new Converter4[B, C, D, E, Record](t)

  def projectionArity = converter.arity

  def fromDb(rs: ResultSet, offset: Int = 0): Record =
    converter.fromDb(rs, offset)
}
