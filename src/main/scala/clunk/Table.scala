package clunk

import scala.collection.mutable.ArrayBuffer
import clunk.Ast.Node._

abstract class Table(val srcName: String) extends TableLike {
  type Self = this.type

  val columns = ArrayBuffer[Column[_]]()
  val associations = ArrayBuffer[Association[Self, _, _]]()
  lazy val selectNode = TableSelectNode(this, columns.toSeq)

  def column[A](name: String)(implicit t: ColumnBuilder[A]) = {
    val targetColumn = t.build(this, name)
    columns += targetColumn

    targetColumn
  }

  def oneToOne[T2 <: Table, A](target: T2, fk: Column[A], pk: Column[A]) =
    makeAssociation[T2, A](target, fk, pk)

  def oneToMany[T2 <: Table, A](target: T2, fk: Column[A], pk: Column[A]) =
    makeAssociation[T2, A](target, fk, pk)

  def manyToOne[T2 <: Table, A](target: T2, fk: Column[A], pk: Column[A]) =
    makeAssociation[T2, A](target, fk, pk)

  override def toString(): String = {
    val builder = new StringBuilder(s"CREATE TABLE ${srcName} (\n")

    columns.map({ c => c.typeTag match {
        case TypeTag.IntTag => s"${c.srcName} INT NOT NULL"
        case TypeTag.StrTag => s"${c.srcName} VARCHAR(255) NOT NULL"
      }
    }).addString(builder, ",\n").append(");").toString
  }

  private def makeAssociation[T2 <: Table, A](t: T2, fk: Column[A], pk: Column[A]) = {
    val association = new Association[Self, T2, A](this, t, fk, pk)
    associations += association

    association
  }
}
