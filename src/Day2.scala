import scala.util.matching.Regex

object Day2 extends Base(2) {

  val Parser: Regex = """(\d+)-(\d+) (\w): (\S+)""".r

  override def part1: Int = // 445
    inputLines.count {
      case Parser(min, max, char, password) =>
        val range = min.toInt to max.toInt
        range.contains(password.count(_ == char.head))
    }

  override def part2: Int = // 491
    inputLines.count {
      case Parser(pos1, pos2, char, password) =>
        val char1 = password(pos1.toInt - 1)
        val char2 = password(pos2.toInt - 1)
        char1 != char2 &&
          (char1 == char.head ||
            char2 == char.head)
    }
}
