import scala.annotation.tailrec

object Day9 extends Base(9) {

  private val numbers = inputLines.map(_.toLong)

  def isNotSum(n: Long, nums: List[Long]): Boolean =
    {
      for {
        a <- nums
        b <- nums
        if a != b
        if a + b == n
      } yield (a, b)
    }.isEmpty

  @tailrec
  def findSum(n: Long, nums: List[Long], acc: List[Long] = Nil): Option[Long] =
    if (acc.sum == n)
      Some(acc.min + acc.max)
    else if (acc.sum > n || nums.isEmpty)
      None
    else
      findSum(n, nums.tail, nums.head :: acc)

  override def part1: Long = // 32321523
    numbers
      .sliding(26)
      .collectFirst { case nums :+ n if isNotSum(n, nums) => n }
      .head

  override def part2: Long = { // 4794981
    val n = part1
    numbers
      .indices
      .view
      .flatMap { index => findSum(n, numbers.drop(index)) }
      .head
  }
}
