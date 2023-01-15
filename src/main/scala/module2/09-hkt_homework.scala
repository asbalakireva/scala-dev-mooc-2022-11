package module2

object hktImplicits {


  trait Bindable[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]

    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  }


  def tupleF[F[_], A, B](fa: F[A], fb: F[B])
                        (implicit F: Bindable[F]): F[(A, B)] =
    F.flatMap(fa) { a =>
      F.map(fb)((a, _))
    }


object Bindable {

    implicit def optBindable: Bindable[Option] = new Bindable[Option] {
      override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)

      override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    }

    implicit def listBindable: Bindable[List] = new Bindable[List] {
      override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)

      override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
    }

  }

  val optA: Option[Int] = Some(1)
  val optB: Option[Int] = Some(2)
  val list1 = List(1, 2, 3)
  val list2 = List(4, 5, 6)
  val r1 = println(tupleF(optA, optB))
  val r2 = println(tupleF(list1, list2))


}

