object Day7 extends Base(7) {

  private val BagPattern = """^(.+) bags contain (.+).$""".r
  private val ContainsPattern = """(\d+) ([^,]+) bags?""".r

  private val bags = inputLines
    .map {
      case BagPattern(color, contains) =>
        color -> Bag(
          color, ContainsPattern
            .findAllMatchIn(contains)
            .map { m => m.group(2) -> m.group(1).toInt }
            .toMap)
    }
    .toMap

  case class Bag(color: String, contains: Map[String, Int]) {

    def contains(color: String): Boolean =
      contains.contains(color) || contains.keysIterator.map(bags.apply).exists(_.contains(color))

    def count: Int =
      contains.view.map { case (color, count) => count + count * bags(color).count }.sum
  }

  override def part1: Int = // 148
    bags.valuesIterator.count(_.contains("shiny gold"))

  override def part2: Int = // 24867
    bags("shiny gold").count
}
