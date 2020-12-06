object Day6 extends Base(6) {

  case class Person(yes: Set[Char])
  case class Group(persons: List[Person] = Nil) {
    def yes: Set[Char] = persons.flatMap(_.yes).toSet
    def consensus: Set[Char] = persons.foldLeft(yes) {
      case (yes, person) =>
        yes.intersect(person.yes)
    }
  }

  def groups(
    group: Group,
    list: List[String]
  ): List[Group] =
    list match {
      case Nil =>
        List(group)
      case "" :: rest =>
        group :: groups(Group(), rest)
      case yes :: rest =>
        val persons = Person(yes.toSet) :: group.persons
        groups(group.copy(persons = persons), rest)
    }

  override def part1: Int = // 6565
    groups(Group(), inputLines).map(_.yes.size).sum

  override def part2: Int = // 3137
    groups(Group(), inputLines).map(_.consensus.size).sum
}
