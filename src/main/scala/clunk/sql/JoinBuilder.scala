package clunk.sql

import clunk.Ast.Node._

class JoinBuilder(join: Option[JoinNode]) {

  def toSql(builder: StringBuilder): StringBuilder =
    join.map(_.tableJoins.map(buildJoin(_)).addString(builder, "")).
      getOrElse(builder)

  private def buildJoin(tj: TableJoinNode) =
    s"""
      |INNER JOIN `${tj.right.srcName}`
      |ON `${tj.left.srcName}`.`${tj.fk.srcName}`
      |= `${tj.right.srcName}`.`${tj.pk.srcName}`""".
      stripMargin.replaceAll("\n", " ")
}
