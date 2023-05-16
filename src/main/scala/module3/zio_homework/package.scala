package module3

import module3.zioConcurrency.{currentTime, printEffectRunningTime}
import module3.zio_homework.PrintEffectRunningTime2
import zio.{Has, Task, ULayer, URIO, ZIO, ZLayer}
import zio.clock.{Clock, sleep}
import zio.console._
import zio.duration.durationInt
import zio.macros.accessible
import zio.random._

import java.io.IOException
import java.util.concurrent.TimeUnit
import scala.io.StdIn
import scala.language.postfixOps

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в когнсоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */



  lazy val guessProgram: ZIO[Console with Random, IOException, Unit] = for {
    random <- nextIntBetween(1, 4)
    _ <- putStrLn(s"Угадайте число от 1 до 3")
    num <- getStrLn.flatMap(input => ZIO.effect(input.toInt)).orDie
    _ <- putStrLn(if (num != random)  s"Не угадали, загадано число $random" else "Угадали!")
  } yield ()

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   * 
   */

  def doWhile[R, E, A](effect: ZIO[R, E, A])(condition: A => Boolean): ZIO[R, Any, Unit] =
  effect.flatMap(x =>
    if (condition(x)) ZIO.succeed()
    else ZIO.effect(println("Условие не выполнено, попробуйте ещё раз")) *> doWhile(effect)(condition))


  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */


  def loadConfigOrDefault: URIO[Console, config.AppConfig]= config.load.orElse{
    val defaultConfig = config.AppConfig("192.0.2.1","8081")
    zio.console.putStr(defaultConfig.toString).as(defaultConfig)
  }



  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */

  val eff: URIO[Random with Clock, Int] = ZIO.sleep(1 second) zipRight nextIntBetween(1, 11)
  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects: URIO[Random with Clock, List[Int]] = ZIO.collectAll(List.fill(10)(eff))

  
  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекци "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */


  lazy val app: URIO[Clock with Console with Random, Int] = printEffectRunningTime(
    for {
    list <- effects
    sum = list.sum
    _ <- putStrLn(s"$sum")
  } yield sum
  )


  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val appSpeedUp = printEffectRunningTime(
    for {
      list <- ZIO.foreachParN(5)(Range(1,11).toList){_ => eff}
      sum = list.sum
      _ <- putStrLn(s"$sum")
    } yield sum
  )


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * молжно было использовать аналогично zio.console.putStrLn например
   */

  type PrintEffectRunningTime2 = Has[PrintEffectRunningTime2.Service]


  object PrintEffectRunningTime2 {
    trait Service {
      def printEffectRunningTime2[R, E, A](effect: ZIO[R, E, A]): ZIO[Console with Clock with R, E, A]
    }

    object Service {
         def printEffectRunningTime2[R, E, A](effect: ZIO[R, E, A]): ZIO[Console with Clock with R, E, A] = for {
          start <- currentTime
          z <- effect
          finish <- currentTime
          _ <- putStrLn(s"Running time from service: ${finish - start}")
        } yield z
      }



    val live: ULayer[PrintEffectRunningTime2] = ZLayer.succeed(new Service {
      override def printEffectRunningTime2[R, E, A](effect: ZIO[R, E, A]): ZIO[Console with Clock with R, E, A] =
        printEffectRunningTime2(effect)
    }
    )
  }




   /**
     * 6.
     * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
     *
     * 
     */

   lazy val appWithTimeLogg: ZIO[PrintEffectRunningTime2 with Console with Clock with Random, Throwable, Int] =
     PrintEffectRunningTime2.Service.printEffectRunningTime2(appSpeedUp)


/**
    * 
    * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
    */

  lazy val runApp = appWithTimeLogg.provideSomeLayer[Console with Random with Clock](PrintEffectRunningTime2.live)

}
