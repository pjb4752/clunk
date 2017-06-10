package clunk

import clunk.Ast.Node._
import clunk.Ast.Node.Comparator._
import clunk.jdbc.Database
import clunk.queries.{Insert, Select, Select2, Update}
import clunk.sql.QueryBuilder

class Query[T1 <: Table](val source: T1) {

  def where(f: T1 => Comparison[_, _]) = {
    val newWhere = queries.Builder.buildWhere(None, f(source))
    new Select(source, selectNode, None, newWhere)
  }

  def innerJoin[T2 <: Table, A](f: T1 => Association[T1, T2, A]) = {
    val association = f(source)
    val newSource = (source, association.right)
    val newJoin = queries.Builder.buildJoin(None, association)

    new Select2(newSource, selectNode, newJoin, None)
  }

  def result = new Select(source, selectNode, None, None).result

  def insert(record: source.Record) = new Insert().execute(source)(record)

  def update(record: source.Record) = new Update().execute(source)(record)

  private def selectNode = SelectNode(Seq(source.selectNode))
}

object Query {

  def apply[T1 <: Table](t: T1) = new Query(t)
}
