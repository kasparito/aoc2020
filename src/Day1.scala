object Day1 extends Base(1) {

  val inputNumbers: Seq[List[Int]] = inputLines.map(n => List(n.toInt))

  override val part1: Int = // 712075
    inputNumbers
      .flatMap(
        a => inputNumbers.map(
          b => a ++ b))
      .find(_.sum == 2020)
      .map { case List(a, b) => a * b }
      .get

  override val part2: Int = // 145245270
    inputNumbers
      .flatMap(
        a => inputNumbers.flatMap(
          b => inputNumbers.map(
            c => a ++ b ++ c)))
      .find(_.sum == 2020)
      .map { case List(a, b, c) => a * b * c }
      .get
}
