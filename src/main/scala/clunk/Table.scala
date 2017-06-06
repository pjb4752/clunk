package clunk

import scala.collection.mutable.ArrayBuffer
import clunk.Ast.Node._

abstract class Table(val srcName: String) extends TableLike {
  type Self = this.type

  val columns = ArrayBuffer[Column[_, _]]()
  val associations = ArrayBuffer[Association[Self, _, _]]()
  lazy val selectNode = TableSelectNode(this, columns.toSeq)

  def column[A](name: String)(implicit t: ColumnBuilder[A]) = {
    val targetColumn = t.build(this, name)
    columns += targetColumn

    targetColumn
  }

  def oneToOne[T2 <: Table, A](target: T2, fk: Column[_, A], pk: Column[_, A]) =
    makeAssociation[T2, A](target, fk, pk)

  def oneToMany[T2 <: Table, A](target: T2, fk: Column[_, A], pk: Column[_, A]) =
    makeAssociation[T2, A](target, fk, pk)

  def manyToOne[T2 <: Table, A](target: T2, fk: Column[_, A], pk: Column[_, A]) =
    makeAssociation[T2, A](target, fk, pk)

  override def toString(): String =
    columns.map(_.toString()).
      mkString(s"CREATE TABLE ${srcName} (\n", ",\n", ");")

  private def makeAssociation[T2 <: Table, A](t: T2, fk: Column[_, A], pk: Column[_, A]) = {
    val association = new Association[Self, T2, A](this, t, fk, pk)
    associations += association

    association
  }
}
