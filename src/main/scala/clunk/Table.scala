package clunk

import scala.collection.mutable.ArrayBuffer
import clunk.Ast.Node._

class Table[A](val srcName: String) extends TableLike[A] {
  type Self = this.type

  val columns = ArrayBuffer[Column[_]]()
  val associations = ArrayBuffer[Association[Self, _, _]]()
  lazy val selectNode = TableSelectNode(this, columns.toSeq)

  def column[A](name: String)(implicit t: ColumnBuilder[A]) = {
    val targetColumn = t.build(this, name)
    columns += targetColumn

    targetColumn
  }

  def oneToOne[B <: Table[_], C](target: B, fk: Column[C], pk: Column[C]) =
    makeAssociation[B, C](target, fk, pk)

  def oneToMany[B <: Table[_], C](target: B, fk: Column[C], pk: Column[C]) =
    makeAssociation[B, C](target, fk, pk)

  def manyToOne[B <: Table[_], C](target: B, fk: Column[C], pk: Column[C]) =
    makeAssociation[B, C](target, fk, pk)

  override def toString(): String = {
    val builder = new StringBuilder(s"CREATE TABLE ${srcName} (\n")

    columns.map({ c => c.typeTag match {
        case TypeTag.IntTag => s"${c.srcName} INT NOT NULL"
        case TypeTag.StrTag => s"${c.srcName} VARCHAR(255) NOT NULL"
      }
    }).addString(builder, ",\n").append(");").toString
  }

  private def makeAssociation[B <: Table[_], C](t: B, fk: Column[C], pk: Column[C]) = {
    val association = new Association[Self, B, C](this, t, fk, pk)
    associations += association

    association
  }
}
