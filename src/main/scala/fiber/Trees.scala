package fiber

trait Trees { self: Base =>

  sealed trait Tree
//  case class Vec(contents: List[Expr]) extends Expr
  case class ListE(contents: List[Tree], isBracket: Boolean) extends Tree {
    override def toString: String =
      if (isBracket) contents.mkString("[", " ", "]")
      else contents.mkString("(", " ", ")")
  }
  case class IntE(value: Int) extends Tree {
    override def toString: String = value.toString
  }
  case class StrE(value: String) extends Tree {
    override def toString: String = value
  }
  case class IdE(id: String) extends Tree {
    override def toString: String = id
  }
  case class Quote(quoted: Tree) extends Tree {
    override def toString: String = s"'$quoted"
  }

}
