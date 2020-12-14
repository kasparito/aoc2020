import scala.collection.concurrent.TrieMap

object Day10 extends Base(10) {

  private val adapters = {
    val sorted = inputLines.map(_.toInt).sorted
    0 +: sorted :+ sorted.last + 3
  }

  override def part1: Int = { // 2176
    val diffs = adapters
      .sliding(2)
      .map { case List(x1, x2) => x2 - x1 }
      .toList
      .groupBy(identity)
      .view
      .mapValues(_.size)
      .toMap
    val ones = diffs.getOrElse(1, 0)
    val threes = diffs.getOrElse(3, 0)
    ones * threes
  }

  private val cache = TrieMap.empty[List[Int], BigInt]

  def countArrangements(adapters: List[Int]): BigInt =
    cache.getOrElseUpdate(adapters, adapters match {
      case first :: second :: Nil if second - first <= 3 =>
        1
      case first :: second :: tail if second - first <= 3 =>
        countArrangements(first :: tail) + countArrangements(second :: tail)
      case _ =>
        0
    })

  private val fib: Stream[BigInt] =
    BigInt(0) #:: BigInt(1) #:: BigInt(1) #::
      fib.zip(fib.tail).zip(fib.tail.tail).map { case ((x, y), z) => x + y + z }

  def countArrangements2(value: Iterator[Int]): BigInt =
    if (value.hasNext) {
      value.dropWhile(_ == 3)
      val ones = value.takeWhile(_ == 1).size + 1
      fib(ones) * countArrangements2(value)
    } else {
      1
    }

  private val adapters1 = List(
    0,
    16,
    10,
    15,
    5,
    1,
    11,
    7,
    19,
    6,
    12,
    4,
    22).sorted

  private val adapters2 = List(
    0,
    28,
    33,
    18,
    42,
    31,
    14,
    46,
    20,
    48,
    47,
    24,
    23,
    49,
    45,
    19,
    38,
    39,
    11,
    1,
    32,
    25,
    35,
    8,
    17,
    7,
    9,
    4,
    2,
    34,
    10,
    3,
    52).sorted

  override def part2: BigInt = // 18512297918464
    countArrangements2(
      adapters
        .sliding(2)
        .map { case List(x1, x2) => x2 - x1 }
        .iterator)
}
