import scala.annotation.tailrec

object Day3 extends Base(3) {

  private val grid = inputLines
  private val tileWidth = inputLines.head.length

  @tailrec
  private def countTrees(x: Int, y: Int, dx: Int, dy: Int, trees: BigInt): BigInt =
    if (y >= grid.length)
      trees
    else
      countTrees(
        x + dx, y + dy,
        dx, dy,
        trees + (if (grid(y)(x % tileWidth) == '#') 1 else 0))

  private def countTrees(dx: Int, dy: Int): BigInt =
    countTrees(0, 0, dx, dy, 0)

  override def part1: BigInt = // 228
    countTrees(3, 1)

  override def part2: BigInt = // ???
    countTrees(1, 1) *
      countTrees(3, 1) *
      countTrees(5, 1) *
      countTrees(7, 1) *
      countTrees(1, 2)
}
