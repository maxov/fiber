package fiber

trait Base {

  sealed trait InputSource
  case class ReplSource(count: Int) extends InputSource {
    override def toString: String = s"_i$count"
  }

  case class Pos(inputType: InputSource, line: Int, char: Int) {
    override def toString: String = s"$line:$char"
  }

}
