package clunk

import scala.collection.mutable.ArrayBuffer
import clunk.Ast.Node._

class Table[A](val srcName: String) extends TableLike[A] {
  val columns = ArrayBuffer[Column[_]]()
  val associations = ArrayBuffer[Association[_]]()
  lazy val selectNode = SelectNode(columns.toSeq)

  def column[A](name: String)(implicit t: ColumnBuilder[A]) = {
    val targetColumn = t.build(name)
    columns += targetColumn

    targetColumn
  }

  def oneToOne[A](target: Table[_], fk: Column[A], pk: Column[A]) =
    makeAssociation[A](target, fk, pk)

  def oneToMany[A](target: Table[_], fk: Column[A], pk: Column[A]) =
    makeAssociation[A](target, fk, pk)

  def manyToOne[A](target: Table[_], fk: Column[A], pk: Column[A]) =
    makeAssociation[A](target, fk, pk)

  override def toString(): String = {
    val builder = new StringBuilder(s"CREATE TABLE ${srcName} (\n")

    columns.map({ c => c.typeTag match {
        case TypeTag.IntTag => s"${c.srcName} INT NOT NULL"
        case TypeTag.StrTag => s"${c.srcName} VARCHAR(255) NOT NULL"
      }
    }).addString(builder, ",\n").append(");").toString
  }

  private def makeAssociation[A](t: Table[_], fk: Column[A], pk: Column[A]) = {
    val association = new Association[A](this, t, fk, pk)
    associations += association

    association
  }
}
