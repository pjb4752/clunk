package clunk

class Association[A](val left: Table[_], val right: Table[_],
  val fk: Column[A], val pk: Column[A])
