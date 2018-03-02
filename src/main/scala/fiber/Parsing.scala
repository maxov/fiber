package fiber

import scala.collection.mutable.ListBuffer

trait Parsing { this: Lexing with Trees with Base =>

  def parse(tokens: List[Token], source: InputSource): List[Tree] = {
    var toks = tokens
    val exprs = ListBuffer[Tree]()
    while (toks.nonEmpty) {
      val (e, rest) = parseExpr(toks)
      toks = rest
      exprs += e
    }
    exprs.toList
  }

  def parseExpr(tokens: List[Token]): (Tree, List[Token]) = {

  }

}
