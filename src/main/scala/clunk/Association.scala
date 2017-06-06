package clunk

class Association[+T1 <: Table, T2 <: Table, A](val left: T1, val right: T2,
  val fk: Column[_, A], val pk: Column[_, A])
