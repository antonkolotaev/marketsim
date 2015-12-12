package marketsim

package object ops {

    import language.implicitConversions

    trait Conversion[-From, +To] {
        def convert(from: From): To
    }

    object Conversions {

        /*implicit def toOption[T, U](implicit ev: Conversion[T, U]): Conversion[T, Option[U]] =
            new Conversion[T, Option[U]] {
                def convert(x: T) = Some(ev.convert(x))
            } */

        implicit def toOptionId[T]: Conversion[T, Option[T]] =
            new Conversion[T, Option[T]] {
                def convert(x: T) = Some(x)
            }

        implicit def betweenOptions[T, U](implicit ev: Conversion[T, U]): Conversion[Option[T], Option[U]] =
            new Conversion[Option[T], Option[U]] {
                def convert(x: Option[T]) = x map { y => ev.convert(y) }
            }

        implicit val int2double = new Conversion[Int, Double] {
            def convert(x: Int) = x
        }
        implicit val int2long = new Conversion[Int, Long] {
            def convert(x: Int) = x
        }
        implicit val long2double = new Conversion[Long, Double] {
            def convert(x: Long) = x
        }

    }

    trait PlusDefined[A, B] {
        type Ret

        def plus(a: A, b: B): Ret
    }

    trait Addable_Level1 {
        implicit def rightConversion[A, B](implicit c: Conversion[A, B], ev: PlusDefined[B, B]): PlusDefined[A, B] =
            new PlusDefined[A, B] {
                type Ret = PlusDefined[B, B]#Ret

                def plus(a: A, b: B) = ev.plus(c.convert(a), b)
            }

        implicit def leftConversion[A, B](implicit c: Conversion[B, A], ev: PlusDefined[A, A]): PlusDefined[A, B] =
            new PlusDefined[A, B] {
                type Ret = PlusDefined[A, A]#Ret

                def plus(a: A, b: B) = ev.plus(a, c.convert(b))
            }
    }

    trait Addable_Level2 extends Addable_Level1 {
        implicit def rightOpt[A, B](implicit ev: PlusDefined[A, B]): PlusDefined[A, Option[B]] =
            new PlusDefined[A, Option[B]] {
                type Ret = Option[PlusDefined[A, B]#Ret]

                def plus(a: A, b: Option[B]) = b map { x => ev.plus(a, x) }
            }

        implicit def leftOpt[A, B](implicit ev: PlusDefined[A, B]): PlusDefined[Option[A], B] =
            new PlusDefined[Option[A], B] {
                type Ret = Option[PlusDefined[A, B]#Ret]

                def plus(a: Option[A], b: B) = a map { x => ev.plus(x, b) }
            }

        implicit def bothOpt[A, B](implicit ev: PlusDefined[A, B]): PlusDefined[Option[A], Option[B]] =
            new PlusDefined[Option[A], Option[B]] {
                type Ret = Option[PlusDefined[A, B]#Ret]

                def plus(a: Option[A], b: Option[B]) =
                    (a, b) match {
                        case (Some(x), Some(y)) => Some(ev.plus(x, y))
                        case _ => None
                    }
            }
    }

    object Addable extends Addable_Level1 {
        implicit def fromNumeric[T: Numeric]: PlusDefined[T, T] = new PlusDefined[T, T] {
            type Ret = T

            def plus(a: T, b: T) = implicitly[Numeric[T]].plus(a, b)
        }

        implicit def fromSignal[T : Numeric] : PlusDefined[reactive.Signal[T],reactive.Signal[T]] = new PlusDefined[reactive.Signal[T],reactive.Signal[T]] {
            type Ret = reactive.Signal[T]

            def plus(a: reactive.Signal[T], b: reactive.Signal[T]) =
                reactive.Binary(a,b,"+") { case (x,y) => implicitly[Numeric[T]].plus(x, y) }

        }
    }

    implicit class RichValue[A](a: reactive.Signal[A]) {
        def and[B](b: reactive.Signal[B]) = reactive.Binary(a, b, "and") { case (x, y) => (x, y) }

        def delayed(dt: Duration) = new Delay(a, dt)
    }

    implicit class OrderingValue[A: Ordering](a: reactive.Signal[A]) {
        def <(b: reactive.Signal[A]) = reactive.Binary(a, b, "<") { case (x, y) => implicitly[Ordering[A]].lt(x, y) }
        def >(b: reactive.Signal[A]) = reactive.Binary(a, b, ">") { case (x, y) => implicitly[Ordering[A]].gt(x, y) }
    }

    class Delay[A](a: reactive.Signal[A], dt: Duration) extends reactive.Variable[A](a(), s"$a delayed $dt") {
        override lazy val inputs = a :: Nil

        override def notifyExternalListenersIfValueChanged(): Unit = {
            val currentValue = a()
            //println(s"T = ${Scheduler.currentTime}; scheduling ${this} <- ${currentValue}")
            Scheduler.afterAgain(dt) {
                this set currentValue
                //val t = Scheduler.currentTime
                //val label = toString
                //println(s"T = $t; ${this} <- ${currentValue}")
            }
        }
    }

}

