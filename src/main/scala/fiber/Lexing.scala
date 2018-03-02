package fiber

import scala.collection.mutable.ListBuffer
import scala.util.Try
import scala.util.matching.Regex

trait Lexing { this: Base =>

  sealed trait Token {
    def pos: Pos
  }
  case class EOF(pos: Pos) extends Token
  case class Id(id: String, pos: Pos) extends Token
  case class Integral(i: Int, pos: Pos) extends Token
  case class Str(s: String, pos: Pos) extends Token
  case class Punct(c: Char, pos: Pos) extends Token

  val LineRegex: Regex = raw"^\n".r
  val WhitespaceRegex: Regex = raw"^[\r\t\f\v ]+".r
  val PunctRegex: Regex = raw"^[()'\[\]]".r
  val IntRegex: Regex = raw"^\d+".r
  val IdRegex: Regex = raw"^\w+".r
  val StrRegex: Regex = "^\"([^\"\\\\]|\\\\.)*\"".r

  val allRegex = List(LineRegex, WhitespaceRegex, PunctRegex, IntRegex, StrRegex, IdRegex)

  /**
    * Attempt to match all the regexes, returning the first one that does, or None otherwise.
    *
    * The object returned is a tuple (regex, matched string, remaining string)
    */
  def tryMatch(in: String): Option[(Regex, String, String)] =
    allRegex.foldLeft(Option.empty[(Regex, String, String)]) { (cur, r) =>
      cur.orElse {
        r.findFirstIn(in).map { s =>
          (r, s, in.substring(s.length))
        }
      }
    }


  /**
    * Consume the input and produce a list of tokens
    */
  def lex(input: String, inputSource: InputSource): List[Token] = {
    var tok = ListBuffer[Token]()
    var in = input
    var curLine = 0
    var curChar = 0
    while (in != "") {
      val pos = Pos(inputSource, curLine, curChar)
      tryMatch(in) match {
        case Some((LineRegex, _, rest)) =>
          in = rest
          curLine += 1
          curChar = 0
        case Some((WhitespaceRegex, m, rest)) =>
          in = rest
          curChar += m.length
        case Some((PunctRegex, m, rest)) if m.length == 1 =>
          in = rest
          curChar += m.length
          tok += Punct(m.charAt(0), pos)
        case Some((IntRegex, m, rest)) if Try { m.toInt }.isSuccess =>
          in = rest
          curChar += m.length
          tok += Integral(m.toInt, pos)
        case Some((StrRegex, m, rest)) =>
          in = rest
          curChar += m.length
          tok += Str(m, pos)
        case Some((IdRegex, m, rest)) =>
          in = rest
          curChar += m.length
          tok += Id(m, pos)
        case None =>
          println(s"lex error at pos $pos")
      }
    }
    tok.toList
  }


}
