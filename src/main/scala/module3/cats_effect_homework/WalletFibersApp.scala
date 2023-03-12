package module3.cats_effect_homework

import cats.effect.{IO, IOApp, Spawn}
import cats.implicits._

import scala.concurrent.duration.DurationInt

// Поиграемся с кошельками на файлах и файберами.

// Нужно написать программу где инициализируются три разных кошелька и для каждого из них работает фоновый процесс,
// который регулярно пополняет кошелек на 100 рублей раз в определенный промежуток времени. Промежуток надо сделать разный, чтобы легче было наблюдать разницу.
// Для определенности: первый кошелек пополняем раз в 100ms, второй каждые 500ms и третий каждые 2000ms.
// Помимо этих трёх фоновых процессов (подсказка - это файберы), нужен четвертый, который раз в одну секунду будет выводить балансы всех трех кошельков в консоль.
// Основной процесс программы должен просто ждать ввода пользователя (IO.readline) и завершить программу (включая все фоновые процессы) когда ввод будет получен.
// Итого у нас 5 процессов: 3 фоновых процесса регулярного пополнения кошельков, 1 фоновый процесс регулярного вывода балансов на экран и 1 основной процесс просто ждущий ввода пользователя.

// Можно делать всё на IO, tagless final тут не нужен.

// Подсказка: чтобы сделать бесконечный цикл на IO достаточно сделать рекурсивный вызов через flatMap:
// def loop(): IO[Unit] = IO.println("hello").flatMap(_ => loop())
object WalletFibersApp extends IOApp.Simple {

  def loop(wallet: Wallet[IO], amount: BigDecimal, duration: Int): IO[Unit] =
    (wallet.topup(amount) *> IO.sleep(duration.millis))
      .flatMap(_ => loop(wallet, amount, duration))

  def print(wallet1: Wallet[IO], wallet2: Wallet[IO], wallet3: Wallet[IO], duration: Int): IO[Unit] =
    for {
      w1 <- wallet1.balance
      w2 <- wallet2.balance
      w3 <- wallet3.balance
      _ <- IO.println(s"wallet1: $w1")
      _ <- IO.println(s"wallet2: $w2")
      _ <- IO.println(s"wallet3: $w3")
      _ <- IO.sleep(duration.millis)
      _ <- print(wallet1, wallet2, wallet3, duration)
    } yield ()


  def run: IO[Unit] =
    for {
      _ <- IO.println("Press any key to stop...")
      wallet1 <- Wallet.fileWallet[IO]("1")
      wallet2 <- Wallet.fileWallet[IO]("2")
      wallet3 <- Wallet.fileWallet[IO]("3")
      // todo: запустить все файберы и ждать ввода от пользователя чтобы завершить работу
      _ <- Spawn[IO].start(loop(wallet1, 100, 100))
      _ <- Spawn[IO].start(loop(wallet2, 100, 200))
      _ <- Spawn[IO].start(loop(wallet3, 100, 300))
      _ <- Spawn[IO].start(print(wallet1, wallet2, wallet3, 1000))
      _ <- IO.readLine.iterateWhile(z => z.length == 0)
    } yield ()

}