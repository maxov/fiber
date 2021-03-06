package fiber

object Fiber extends Base with Lexing with Parsing with Trees {

  def main(args: Array[String]): Unit = {
    val in = ReplSource(0)
    println(parse(lex(
      """(defn x [y z] (+ y z))
        |(defn a [] 3)""".stripMargin, in), in))
  }

}
