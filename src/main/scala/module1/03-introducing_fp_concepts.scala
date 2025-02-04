package module1

import java.util.UUID

import scala.annotation.tailrec
import java.time.Instant


import scala.language.postfixOps



/**
 * referential transparency
 */


 object referential_transparency{

  case class Abiturient(id: String, email: String, fio: String)

  type Html = String

  sealed trait Notification

  object Notification{
    case class Email(email: String, text: Html) extends Notification
    case class Sms(telephone: String, msg: String) extends Notification
  }


  case class AbiturientDTO(email: String, fio: String, password: String)

  trait NotificationService{
    def sendNotification(notification: Notification): Unit
    def createNotification(abiturient: Abiturient): Notification
  }


  trait AbiturientService{

    def registerAbiturient(abiturientDTO: AbiturientDTO): Abiturient
  }

  class AbiturientServiceImpl(val notificationService: NotificationService) extends AbiturientService{
    override def registerAbiturient(abiturientDTO: AbiturientDTO): Abiturient = {
      val notification = Notification.Email("", "")
      val abiturient = Abiturient(UUID.randomUUID().toString, abiturientDTO.email, abiturientDTO.fio)
      //notificationService.sendNotification(notification)
      // save(abiturient)
      abiturient
    }
  }


}


 // recursion

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

  def fact(n: Int): Int = {
    var _n = 1
    var i = 2
    while (i <= n){
      _n *= i
      i += 1
    }
    _n
  }



  def factRec(n: Int): Int =
    if( n <= 0) 1 else n * factRec(n - 1)

  def factTailRec(n: Int): Int = {
    @tailrec
    def loop(_n: Int, acc: Int): Int =
      if(_n <= 1) acc
      else loop(_n - 1, _n * acc)

    loop(n, 1)
  }




  /**
   * реализовать вычисление N числа Фибоначчи
   * F0 = 0, F1 = 1, Fn = Fn-1 + Fn - 2
   *
   */


}

object hof{

   trait Consumer{
       def subscribe(topic: String): LazyList[Record]
   }

   case class Record(value: String)

   case class Request()
   
   object Request {
       def parse(str: String): Request = ???
   }

  /**
   *
   * Реализовать ф-цию, которая будет читать записи Request из топика,
   * и сохранять их в базу
   */
   def createRequestSubscription() = {
     val cons: Consumer = ???
     val stream: LazyList[Record] = cons.subscribe("request")
     stream.foreach{ r =>
       val req = Request.parse(r.value)
       // save to DB
     }
   }

   def createSubscription[T](topic: String)(f: LazyList[Record] => T): T = {
     val cons: Consumer = ???
     val stream = cons.subscribe(topic)
     f(stream)
   }

  def createRequestSubscription2() = createSubscription("request"){ l =>
    l.foreach{ r =>
      val req = Request.parse(r.value)
      // save to DB
    }
  }

  

  // обертки

  def logRunningTime[A, B](f: A => B): A => B = a => {
    val start = System.currentTimeMillis()
    val result: B = f(a)
    val end = System.currentTimeMillis()
    println(end - start)
    result
  }


  def doomy(i: Int): Int = {
    Thread.sleep(1000)
    i + 1
  }

  doomy(1) // 2
  val r1 = logRunningTime(doomy)
  r1(1) //2



  // изменение поведения ф-ции

  val arr = Array(1, 2, 3, 4, 5)

  def isOdd(i: Int): Boolean = i % 2 > 0

  def not[A](f: A => Boolean): A => Boolean = a => !f(a)
  
  lazy val isEven: Int => Boolean = not(isOdd)





  // изменение самой функции

  // Follow type implementation

  def partial[A, B, C](a: A, f: (A, B) => C): B => C = b => f(a, b)

  def sum(x: Int, y: Int): Int = x + y

  val p: Int => Int = partial(2, sum)

  p(3) // 5


}






/**
 *  Реализуем тип Option
 */


 object opt {

  /**
   *
   * Реализовать тип Option, который будет указывать на присутствие либо отсутсвие результата
   */

  // Covariant - animal родитель dog, Option[Animal] родитель Option[Dog]


  // Contravariant - animal родитель dog, Option[Dog] родитель Option[Animal]

  // Invariant - нет отношений

  // Вопрос вариантности

  sealed trait Option[+T] {

    def isEmpty: Boolean = this match {
      case Option.Some(v) => false
      case Option.None => true
    }

    def map[B](f: T => B): Option[B] = flatMap(v => Option(f(v)))

    def flatMap[B](f: T => Option[B]): Option[B] = this match {
      case Option.Some(v) => f(v)
      case Option.None => Option.None
    }

    /**
     *
     * Реализовать метод printIfAny, который будет печатать значение, если оно есть
     */

    def printIfAny: Unit = this match {
      case Option.Some(v) => println(v)
      case Option.None => ()
    }

    /**
     *
     * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
     */

//   для реализации zip создала вспомогательный метод, получающий значение из Option
    def get: T = this match {
      case Option.Some(v) => v
      case Option.None => throw new Exception("empty Option")
    }

    def zip[B](option: Option[B]): Option[(T, B)] =
      if (this.isEmpty || option.isEmpty) Option.None
      else Option(this.get, option.get)


    /**
     *
     * Реализовать метод filter, который будет возвращать не пустой Option
     * в случае если исходный не пуст и предикат от значения = true
     */

    def filter(f: T => Boolean): Option[T] = this match {
      case Option.Some(v) if f(v) => Option.Some(v)
      case _ => Option.None

    }


    object Option {

      case class Some[T](v: T) extends Option[T]

      case object None extends Option[Nothing]

      def apply[T](v: T): Option[T] = Some(v)
    }


  }

  object list {

    /**
     *
     * Реализовать односвязанный иммутабельный список List
     * Список имеет два случая:
     * Nil - пустой список
     * Cons - непустой, содердит первый элемент (голову) и хвост (оставшийся список)
     */

    sealed trait List[+T] {
      /**
       * Метод cons, добавляет элемент в голову списка, для этого метода можно воспользоваться названием `::`
       *
       */
      def ::[TT >: T](elem: TT): List[TT] = List.::(elem,this)

      /**
       * Метод mkString возвращает строковое представление списка, с учетом переданного разделителя
       *
       */
      def mkString(sep: String): String = {
        val accum = new StringBuilder()
        @tailrec
        def loop(list: List[T], acc: StringBuilder): StringBuilder = {
          list match {
            case List.Nil => acc
            case List.::(head,List.Nil) => acc.append(head)
            case List.::(head,tail) => loop(tail, acc.append(head.toString.concat(sep)))
          }
        }
        loop(this, accum).toString()
      }



      /**
       *
       * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
       */

      def reverse: List[T] ={
        @tailrec
        def loop (list: List[T], reverseList: List[T]): List[T] = {
          list match {
            case List.Nil => reverseList
            case List.::(head, tail) => loop(tail, head::reverseList)
          }
        }
        loop(this, List.Nil)
      }

      /**
       *
       * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
       */
      def map[B](f:T => B): List[B] = {
        @tailrec
        def loop (list: List[T], acc: List[B]): List[B] = {
          list match {
            case List.Nil => acc
            case List.::(head, tail) => loop(tail, f(head):: acc)
          }
        }
        loop(this, List.Nil).reverse
      }

      /**
       *
       * Реализовать метод filter для списка который будет фильтровать список по некому условию
       */

      def filter(f: T => Boolean): List[T] = {
        @tailrec
        def loop (list: List[T], acc: List[T] = List.Nil): List[T] = {
          list match {
            case List.Nil => acc
            case List.::(head, tail) if f(head) => loop(tail, head :: acc)
            case List.::(_, tail) => loop(tail, acc)
          }
        }
        loop(this).reverse
      }

      //   не совсем поняла задание, реализовала в 2х вариантах. первый, если применять функцию к списку,
      //   ниже, если передавать список в качестве аргумента
      /**
       *
       * Написать функцию incList котрая будет принимать список Int и возвращать список,
       * где каждый элемент будет увеличен на 1
       */

      def incList1: List[Int] = {
        @tailrec
        def loop (list: List[T], acc: List[Int] = List.Nil): List[Int] = {
          list match {
            case List.Nil => acc
            case List.::(head:Int, tail) => loop(tail, head + 1 :: acc)
            case _ => throw new Exception("Not Int List")
          }
        }
        loop(this).reverse
      }
      /**
       *
       * Написать функцию shoutString котрая будет принимать список String и возвращать список,
       * где к каждому элементу будет добавлен префикс в виде '!'
       */

      def shoutString1: List[String] = {
        @tailrec
        def loop (list: List[T], acc: List[String] = List.Nil): List[String] = {
          list match {
            case List.Nil => acc
            case List.::(head:String, tail) => loop(tail, "!".concat(head)::acc )
            case _ => throw new Exception("Not String List")
          }
        }
        loop(this).reverse
      }
    }
    /**
     * Конструктор, позволяющий создать список из N - го числа аргументов
     * Для этого можно воспользоваться *
     *
     * Например вот этот метод принимает некую последовательность аргументов с типом Int и выводит их на печать
     * def printArgs(args: Int*) = args.foreach(println(_))
     */
    object List {

      case class ::[A](head: A, tail: List[A]) extends List[A]
      case object Nil extends List[Nothing]

      def apply[A](v: A*): List[A] =
        if (v.isEmpty) Nil else ::(v.head, apply(v.tail: _*))

    }

    List(1, 2, 3)

//    далее реализованы incList и shoutString , где список должен передаваться именно в качестве аргумента
    /**
     *
     * Написать функцию incList котрая будет принимать список Int и возвращать список,
     * где каждый элемент будет увеличен на 1
     */

    def incList2(list: List[Int]): List[Int] = {
      @tailrec
      def loop (list: List[Int], acc: List[Int]): List[Int] = {
        list match {
          case List.Nil => acc
          case List.::(head, tail) => loop(tail, (head+1)::acc )
        }
      }
      loop(list, List.Nil).reverse
    }

    /**
     *
     * Написать функцию shoutString котрая будет принимать список String и возвращать список,
     * где к каждому элементу будет добавлен префикс в виде '!'
     */

    def shoutString2(list: List[String]): List[String] = {
      @tailrec
      def loop (list: List[String], acc: List[String]): List[String] = {
        list match {
          case List.Nil => acc
          case List.::(head, tail) => loop(tail, "!".concat(head)::acc )
        }
      }
      loop(list, List.Nil).reverse
    }


  }

}