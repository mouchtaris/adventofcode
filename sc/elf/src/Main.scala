import scala.collection.immutable.Map

object gam {
  type LimBy = Iterable[(Char, Int)] => (((Char, Int)) => Int) => (Char, Int)
  type Lim = Seq[Char] => Char

  def common(limit_in_by: LimBy): Lim = { bits =>
    val groups = bits.groupBy(c => c).map { case (b, l) => (b, l.length) }

    limit_in_by(groups)(_._2)._1
  }

  def most_common: Lim = common(_.maxBy)

  def least_common: Lim = common(_.minBy)

  val name_by: Lim => Seq[Seq[Char]] => Seq[Char] = { f => l =>
    l.map(f)
  }

  def read_bin(num: Seq[Char]): Int = Integer.parseInt(num.mkString, 2)

  def make_groups: Seq[Char] => Map[Char, Int] =
    bits => bits.groupBy(c => c).map { case (b, l) => (b, l.length) }

  def find(
      gt: (Int, Int) => Boolean
  )(candi: Seq[Seq[Char]])(i: Int): Seq[Char] = {
    val words = candi.transpose
    val name = words.map { bits =>
      make_groups(bits).toSeq match {
        case Seq((b, l)) => b
        case Seq((b0, l0), (b1, l1)) =>
          if (gt(l0, l1)) b0
          else if (gt(l1, l0)) b1
          else
            (
              if (gt(1, 0)) 1 else 0
            ).toString.head
      }
    }
    val rest = candi.filter(s => s(i) == name(i))
    if (rest.length == 1)
      rest.head
    else
      find(gt)(rest)(i + 1)
  }

  case class ma(input: String = Input.gamma) {
    val readings: Seq[Seq[Char]] =
      input.split("\\s+").toSeq.filterNot(_.isEmpty).map(_.toSeq)

    val bits: Seq[Seq[Char]] = readings.transpose

    def read_name(lim: Lim): Seq[Char] = gam.name_by(lim)(bits)

    val gamma_name: Seq[Char] = name_by(most_common)(bits)
    val epsil_name: Seq[Char] = name_by(least_common)(bits)

    val gamma: Int = read_bin(gamma_name)
    val epsil: Int = read_bin(epsil_name)

    val power_consumpsion: Int = gamma * epsil

    val oxygen_name: Seq[Char] = find(_ > _)(readings)(0)
    val co2_name: Seq[Char] = find(_ < _)(readings)(0)

    val oxygen: Int = read_bin(oxygen_name)
    val co2: Int = read_bin(co2_name)

    val life_support_point: nav.Point = nav.Point(oxygen, co2)
    val life_support_mag: Int = life_support_point.mul
  }
}

object Main {
  val increases = Sonar(Input.sonar).increases(_)
  val ngt = nav.igator()
  val gmm = gam.ma()

  val increases_1 = increases(1)
  val increases_3 = increases(3)
  val hloc_1 = ngt.location1.mul
  val hloc_2 = ngt.location2.mul
  val power = gmm.power_consumpsion
  val life_support = gmm.life_support_mag

  def test = {
    def assert_eq[A, B](a: A, b: B) = {
      if (a != b) {
        throw new Exception(s"$a != $b")
      } else {
        println(s"[OK] $a == $b")
      }
    }
    assert_eq(increases_1, 1316)
    assert_eq(increases_3, 1344)
    assert_eq(hloc_1, 1635930)
    assert_eq(hloc_2, 1781819478)
    assert_eq(power, 4118544)
    assert_eq(life_support, 3832770)
  }

  def main(args: Array[String]) = {
    println(s"Hola.")
    test
    println(s"Increases-1: ${increases_1}")
    println(s"Increases-3: ${increases_3}")
    println(s"Num cmds   : ${ngt.commands.length}")
    println(s"Location   : ${ngt.location1}")
    println(s"HLocation  : ${hloc_1}")
    println(s"Location2  : ${ngt.location2}")
    println(s"HLocation2 : ${hloc_2}")
    println(s"Gamma Rdn# : ${gmm.readings.length}")
    println(s"Gamma Name : ${gmm.gamma_name.mkString}")
    println(s"Epsil Name : ${gmm.epsil_name.mkString}")
    println(s"Gamma      : ${gmm.gamma}")
    println(s"Epsil      : ${gmm.epsil}")
    println(s"PwrConsum  : ${power}")
    println(s"Oxygn Name : ${gmm.oxygen_name.mkString}")
    println(s"CO2   Name : ${gmm.co2_name.mkString}")
    println(s"OxygnRating: ${gmm.oxygen}")
    println(s"CO2  Rating: ${gmm.co2}")
    println(s"Life Supprt: ${life_support}")
  }
}
