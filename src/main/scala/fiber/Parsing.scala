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
    tokens match {
      case Id(id, _) :: rest =>
        (IdE(id), rest)
      case IntT(v, _) :: rest =>
        (IntE(v), rest)
      case StrT(s, _) :: rest =>
        (StrE(s), rest)
      case Punct('\'', _) :: rest =>
        val (e, rest1) = parseExpr(rest)
        (Quote(e), rest1)
      case Punct('(', _) :: rest =>
        val (e, rest1) = parseList(')', rest)
        (e, rest1)
      case Punct('[', _) :: rest =>
        val (e, rest1) = parseList(']', rest)
        (e, rest1)
    }
  }

  def punctMatches(tok: Token, end: Char): Boolean = tok match {
    case Punct(`end`, _) => true
    case _ => false
  }

  def parseList(end: Char, tokens: List[Token]): (Tree, List[Token]) = {
    val contents = ListBuffer[Tree]()
    var toks = tokens
    while (toks.nonEmpty && !punctMatches(toks.head, end)) {
      val (e, rest) = parseExpr(toks)
      contents += e
      toks = rest
    }
    if (toks.isEmpty) {
      throw new IllegalArgumentException("error: unexpected EOF")
    }
    // ignore the next ) or ]
    toks = toks.tail
    val isBracket =
      if (end == ']') true
      else false
    (ListE(contents.toList, isBracket), toks)
  }

}
