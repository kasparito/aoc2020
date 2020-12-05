object Day5 extends Base(5) {

  case class Seat(row: Int, column: Int) {
    val id: Int = row * 8 + column
  }

  object Seat {

    def unapply(seat: String): Option[Seat] = {
      val row = decodeNumber(seat.take(7), 'F', 'B')
      val column = decodeNumber(seat.takeRight(3), 'L', 'R')
      Some(Seat(row, column))
    }

    private def decodeNumber(s: String, zeroChar: Char, oneChar: Char): Int =
      Integer.parseInt(s.replace(zeroChar, '0').replace(oneChar, '1'), 2)
  }

  override def part1: Int = // 848
    inputLines.map { case Seat(seat) => seat.id }.max

  override def part2: Int = // 682
    inputLines
      .map { case Seat(seat) => seat }
      .groupBy(_.row)
      .collect {
        case (row, seats) if seats.size == 7 =>
          val column = ((0 to 7).toSet -- seats.map(_.column)).head
          val seat = Seat(row, column)
          seat.id
      }
      .head
}
