import scala.annotation.tailrec

object Day3 extends Base(3) {

  object Grid {
    private val tile = inputLines
    private val tileWidth = tile.head.length
    val height: Int = tile.length
    def isTree(x: Int, y: Int): Int =
      if (tile(y)(x % tileWidth) == '#') 1 else 0
  }

  private def countTrees(dx: Int, dy: Int): BigInt = {
    @tailrec
    def countTrees(x: Int, y: Int, trees: BigInt): BigInt =
      if (y >= Grid.height)
        trees
      else
        countTrees(
          x + dx, y + dy,
          trees + Grid.isTree(x, y))
    countTrees(0, 0, 0)
  }

  override def part1: BigInt = // 228
    countTrees(3, 1)

  override def part2: BigInt = // 6818112000
    countTrees(1, 1) *
      countTrees(3, 1) *
      countTrees(5, 1) *
      countTrees(7, 1) *
      countTrees(1, 2)
}
