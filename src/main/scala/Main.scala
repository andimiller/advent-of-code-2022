import cats.data.{Ior, NonEmptyList}
import cats.effect.{ExitCode, IO, IOApp}
import cats.parse.Parser
import cats.implicits.*
import utils.Shared

object PartOne extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    for {
      str <- Shared.loadResource("dayx.txt")
      _   <- IO.println(str)
    } yield ExitCode.Success
  }
}

object PartTwo extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    for {
      str <- Shared.loadResource("dayx.txt")
      _   <- IO.println(str)
    } yield ExitCode.Success
  }
}
