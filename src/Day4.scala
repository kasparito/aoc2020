import scala.util.Try

object Day4 extends Base(4) {

  def isValidPassport1(p: Map[String, String]): Boolean =
    p.size == 7 && !p.contains("cid") || p.size == 8

  private val byrRange = Range.inclusive(1920, 2002)
  private val iyrRange = Range.inclusive(2010, 2020)
  private val eyrRange = Range.inclusive(2020, 2030)

  private val hgtInRange = Range.inclusive(59, 76)
  private val hgtCmRange = Range.inclusive(150, 193)
  private def validLength(v: String): Boolean =
    if (v.endsWith("in"))
      validInt(v.dropRight(2), hgtInRange)
    else if (v.endsWith("cm"))
      validInt(v.dropRight(2), hgtCmRange)
    else
      false

  private val hclPattern = """#[0-9a-f]{6}""".r
  private val eclPattern = """amb|blu|brn|gry|grn|hzl|oth""".r
  private val pidPattern = """\d{9}""".r

  private def validInt(v: String, range: Range): Boolean =
    Try(v.toInt).toOption.exists(range.contains)

  def isValidPassport2(p: Map[String, String]): Boolean =
    p.get("byr").exists(validInt(_, byrRange)) &&
      p.get("iyr").exists(validInt(_, iyrRange)) &&
      p.get("eyr").exists(validInt(_, eyrRange)) &&
      p.get("hgt").exists(validLength) &&
      p.get("hcl").exists(hclPattern.matches) &&
      p.get("ecl").exists(eclPattern.matches) &&
      p.get("pid").exists(pidPattern.matches)

  def passports(
    current: Map[String, String],
    list: List[String],
    isValidPassport: Map[String, String] => Boolean
  ): List[Map[String, String]] =
    list match {
      case Nil if isValidPassport(current) =>
        List(current)
      case Nil =>
        Nil
      case "" :: rest if isValidPassport(current) =>
        current :: passports(Map.empty, rest, isValidPassport)
      case "" :: rest =>
        passports(Map.empty, rest, isValidPassport)
      case fields :: rest =>
        val pairs = fields
          .split(' ')
          .map(_.split(':') match { case Array(n, v) => (n, v) })
        passports(current ++ pairs, rest, isValidPassport)
    }

  override def part1: Int =
    passports(Map.empty, inputLines, isValidPassport1).size

  override def part2: Int =
    passports(Map.empty, inputLines, isValidPassport2).size
}
