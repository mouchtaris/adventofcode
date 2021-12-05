case class Sonar[I](input: Iterable[I]) {
  def increases(window: Int) =
    Input.sonar
      .sliding(window)
      .sliding(2)
      .count {
        case Seq(a, b) if (a.sum compare b.sum) == -1 => true
        case _                                        => false
      }
}

