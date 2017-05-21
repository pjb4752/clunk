package clunk

class Association[+A <: Table[_], B <: Table[_], C](val left: A, val right: B,
  val fk: Column[C], val pk: Column[C])
