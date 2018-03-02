package fiber

object Fiber extends Base with Lexing with Parsing with Trees {

  def main(args: Array[String]): Unit = {
    val in = ReplSource(0)
    println(lex(
      """(hello)
        |(how are (you) 3 "yoo")
      """.stripMargin, in))
  }

}
