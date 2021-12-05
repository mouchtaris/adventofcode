import mill._, scalalib._, scalafmt._

object elf
  extends Module
    with ScalaModule
    with ScalafmtModule
{
  def scalaVersion = "2.13.7"

  def scalacOptions = Seq(
    "-Ydelambdafy:inline",
    "-deprecation",
    "-Xsource:3",
  )

  def forkArgs = Seq("-Xmx4g")

  def forkEnv = Map("HELLO_MY_ENV_VAR" -> "WORLD")
}
