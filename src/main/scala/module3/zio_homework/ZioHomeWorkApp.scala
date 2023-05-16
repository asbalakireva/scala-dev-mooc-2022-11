package module3.zio_homework

import zio.clock.Clock
import zio.console.Console
import zio.random.Random
import zio.{ExitCode, Task, URIO, ZIO}

import scala.io.StdIn
import scala.util.Try

object ZioHomeWorkApp extends zio.App {
  override def run(args: List[String]): URIO[Clock with Random with Console, ExitCode] = {
    // 1
    //  guessProgram.exitCode

    // 2
    //  val readLine: Task[Int] = ZIO.fromTry(Try(StdIn.readLine())).flatMap(str => ZIO.effect(str.toInt))
    //  doWhile(readLine)(x => x == 1).exitCode

    // 3
    //  loadConfigOrDefault.exitCode

    // 4
//    app.exitCode
//    appSpeedUp.exitCode

    //  6
    runApp.exitCode
  }
}