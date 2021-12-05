object nav {
  trait Command { def units: Int }
  case class forward(override val units: Int) extends Command
  case class down(override val units: Int) extends Command
  case class up(override val units: Int) extends Command

  val mappings: String => Int => Command = {
    case "forward" => forward
    case "up"      => up
    case "down"    => down
    case other     => throw new Exception(" WHAT IS THIS --${other}--")
  }

  def parse_commands(input: String): Iterator[Command] = {
    input
      .split("\\s+")
      .iterator
      .filterNot(_.isEmpty)
      .grouped(2)
      .withPartial(false)
      .map { case Seq(name, unit) => mappings(name)(unit.toInt) }
  }

  trait Commandable[S] {
    def command(s: S, cmd: Command): S
  }

  def execute[S: Commandable](commands: Iterable[Command])(ship: S): S = {
    val move = implicitly[Commandable[S]].command _
    commands.foldLeft(ship)(move)
  }

  case class Point(x: Int = 0, y: Int = 0) {
    def mul = x * y
    def command: Command => Point = {
      case forward(δ) => copy(x = x + δ)
      case up(δ)      => copy(y = y - δ)
      case down(δ)    => copy(y = y + δ)
    }
  }
  object Point {
    implicit def commandable: Commandable[Point] = (_.command(_))
  }

  case class igation(point: Point = Point(), aim: Int = 0) {
    def command: Command => igation = {
      case forward(δ) =>
        copy(
          point = point.copy(
            x = point.x + δ,
            y = point.y + δ * aim
          )
        )
      case up(δ)   => copy(aim = aim - δ)
      case down(δ) => copy(aim = aim + δ)
    }
  }
  object igation {
    implicit def commandable: Commandable[igation] = (_.command(_))
  }

  case class igator(input: String = Input.navigation) {
    val commands: Seq[Command] = parse_commands(input).toSeq
    val location1: Point = execute(commands)(Point())
    val location2: Point = execute(commands)(igation()).point
  }
}

