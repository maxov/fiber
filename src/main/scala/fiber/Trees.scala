package fiber

trait Trees { self: Base =>

  sealed trait Tree
//  case class Vec(contents: List[Expr]) extends Expr
  case class ListE(contents: List[Tree]) extends Tree
  case class IntegerE(value: Int) extends Tree
  case class StringE(value: String) extends Tree
  case class IdE(id: String) extends Tree
  case class Quote(quoted: Tree) extends Tree

}
