import scala.annotation.tailrec

object Day8 extends Base(8) {

  case class State(position: Int, accumulator: Int)

  sealed trait Instruction {
    def run(state: State): State
  }
  case class Nop(value: Int) extends Instruction {
    override def run(state: State): State = state.copy(
      position = state.position + 1)
  }
  case class Jmp(value: Int) extends Instruction {
    override def run(state: State): State = state.copy(
      position = state.position + value)
  }
  case class Acc(value: Int) extends Instruction {
    override def run(state: State): State = state.copy(
      position = state.position + 1,
      accumulator = state.accumulator + value)
  }

  sealed trait Result
  case class Halt(accumulator: Int) extends Result
  case class Loop(accumulator: Int) extends Result

  class Computer(instructions: IndexedSeq[Instruction]) {

    @tailrec
    final def run(state: State = State(0, 0), visited: Set[Int] = Set.empty): Result =
      instructions(state.position).run(state) match {
        case State(position, accumulator) if position == instructions.size =>
          Halt(accumulator)
        case State(position, accumulator) if visited(position) =>
          Loop(accumulator)
        case state =>
          run(state, visited + state.position)
      }
  }

  private val instructions = {
    val Pattern = """(\w{3}) \+?(-?\d+)""".r
    inputLines.map {
      case Pattern("nop", value) => Nop(value.toInt)
      case Pattern("jmp", value) => Jmp(value.toInt)
      case Pattern("acc", value) => Acc(value.toInt)
    }.toIndexedSeq
  }

  override def part1: Int = // 1928
    new Computer(instructions).run() match {
      case Loop(accumulator) =>
        accumulator
    }

  override def part2: Int = // 1319
    instructions
      .zipWithIndex
      .view
      .collect {
        case (Nop(value), index) =>
          new Computer(instructions.updated(index, Jmp(value))).run()
        case (Jmp(value), index) =>
          new Computer(instructions.updated(index, Nop(value))).run()
      }.collectFirst {
        case Halt(accumulator) =>
          accumulator
      }.head
}
