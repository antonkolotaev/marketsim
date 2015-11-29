package marketsim
package ops

import org.scalatest.FlatSpec
import reactive._

class PlusSpec extends FlatSpec
{

    def add[A,B](a : A, b : B)(implicit e : PlusDefined[A,B]) =
    {
        e.plus(a,b)
    }

    implicit def toOptionId[T]: Conversion[T, Option[T]] =
        new Conversion[T, Option[T]] {
            def convert(x: T) = Some(x)
        }

    implicit def toOptionId_ctx[C,T]: Conversion[C => T, C => Option[T]] =
        new Conversion[C => T, C => Option[T]] {
            def convert(x: C => T) = c => Some(x(c))
        }

    implicit def toOptionId_ctx2[C,T]: Conversion[T, C => Option[T]] =
        new Conversion[T, C => Option[T]] {
            def convert(x: T) = c => Some(x)
        }

    implicit def toSignal[T]: Conversion[T, Signal[T]] =
        new Conversion[T, Signal[T]] {
            def convert(x : T) = new Variable(x, x.toString)
        }

    implicit def toSignal_ctx[C,T]: Conversion[C => T, C => Signal[T]] =
        new Conversion[C => T, C => Signal[T]] {
            def convert(x : C => T) = c => new Variable(x(c), x(c).toString)
        }

    implicit def toOptionSignal[T]: Conversion[T, Signal[Option[T]]] =
        new Conversion[T, Signal[Option[T]]] {
            def convert(x : T) = new Variable(Some(x), x.toString)
        }

    implicit def toOptionSignal2[T]: Conversion[Signal[T], Signal[Option[T]]] =
        new Conversion[Signal[T], Signal[Option[T]]] {
            def convert(x : Signal[T]) = Unary(x,s"$x.opt") { y => Some(y) }
        }


    implicit def toFunction[T]: Conversion[T, () => T] =
        new Conversion[T, () => T] {
            def convert(x : T) = () => x
        }

    implicit def toOptionFunction[T]: Conversion[T, () => Option[T]] =
        new Conversion[T, () => Option[T]] {
            def convert(x : T) = () => Some(x)
        }

    implicit def toOptionFunction2[T]: Conversion[() => T, () => Option[T]] =
        new Conversion[() => T, () => Option[T]] {
            def convert(x : () => T) = () => Some(x())
        }

    implicit def toFunction_sig[T]: Conversion[Signal[T], () => T] =
        new Conversion[Signal[T], () => T] {
            def convert(x : Signal[T]) = x
        }

    implicit def toOptionFunction_sig[T]: Conversion[Signal[T], () => Option[T]] =
        new Conversion[Signal[T], () => Option[T]] {
            def convert(x : Signal[T]) = () => Some(x())
        }


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

    implicit def fromNumeric[T: Numeric]: PlusDefined[T, T] = new PlusDefined[T, T]
    {
        type Ret = T

        def plus(a: T, b: T) = implicitly[Numeric[T]].plus(a, b)
    }

    implicit def fromNumericOption[T: Numeric]: PlusDefined[Option[T], Option[T]] = new PlusDefined[Option[T], Option[T]]
    {
        type Ret = Option[T]

        def plus(a: Option[T], b: Option[T]) = (a,b) match {
            case (Some(x), Some(y)) => Some(implicitly[Numeric[T]].plus(x, y))
            case _ => None
        }
    }

    implicit def fromFunction[T: Numeric]: PlusDefined[() => T, () => T] = new PlusDefined[() => T, () => T]
    {
        type Ret = () => T

        def plus(a: () => T, b: () => T) = () => implicitly[Numeric[T]].plus(a(), b())
    }

    implicit def fromFunctionOption[T: Numeric]: PlusDefined[() => Option[T], () => Option[T]] = new PlusDefined[() => Option[T], () => Option[T]]
    {
        type Ret = () => Option[T]

        def plus(a: () => Option[T], b: () => Option[T]) = () => (a(),b()) match {
            case (Some(x),Some(y)) => Some(implicitly[Numeric[T]].plus(x,y))
            case _ => None
        }
    }

    implicit def fromSignal[T: Numeric]: PlusDefined[Signal[T], Signal[T]] = new PlusDefined[Signal[T], Signal[T]]
    {
        type Ret = Signal[T]

        def plus(a: Signal[T], b: Signal[T]) = Binary(a,b,"+"){ case (x,y) => implicitly[Numeric[T]].plus(x,y) }
    }

    implicit def fromSignalOption[T: Numeric]: PlusDefined[Signal[Option[T]], Signal[Option[T]]] = new PlusDefined[Signal[Option[T]], Signal[Option[T]]]
    {
        type Ret = Signal[Option[T]]

        def plus(a: Signal[Option[T]], b: Signal[Option[T]]) = Binary(a,b,"+") { case (p,r) => (p,r) match
            {
                case (Some(x), Some(y)) => Some(implicitly[Numeric[T]].plus(x, y))
                case _ => None
            }
        }
    }
/*    "1 + 2" should "give 3" in assert(add(1,2) == 3)
    "1L + 2L" should "give 3L" in assert(add(1L,2L) == 3L)
    "1.0 + 2.0" should "give 3.0" in assert(add(1.0,2.0) == 3.0)

    "1 + 2L" should "give 3L" in assert(add(1, 2L) == 3L)
    "1.0 + 2" should "give 3.0" in assert(add(1.0, 2) == 3.0)

    val some1 = Some(1) : Option[Int]
    val some1L = Some(1L) : Option[Long]
    val some2 = Some(2) : Option[Int] */

    val c1 = implicitly[PlusDefined[Int, Option[Int]]]
    val c2 = implicitly[PlusDefined[Option[Int], Int]]

    val s3 = implicitly[PlusDefined[Int, Signal[Int]]]
    val s4 = implicitly[PlusDefined[Signal[Int], Int]]

    val s5 = implicitly[PlusDefined[Int, Signal[Option[Int]]]]
    val s6 = implicitly[PlusDefined[Signal[Option[Int]], Int]]

    val s7 = implicitly[PlusDefined[Signal[Int], Signal[Option[Int]]]]
    val s8 = implicitly[PlusDefined[Signal[Option[Int]], Signal[Int]]]

    val c3 = implicitly[PlusDefined[Int, () => Int]]
    val c4 = implicitly[PlusDefined[() => Int, Int]]

    val c5 = implicitly[PlusDefined[Int, () => Option[Int]]]
    val c6 = implicitly[PlusDefined[() => Option[Int], Int]]

    val c7 = implicitly[PlusDefined[() => Int, () => Option[Int]]]
    val c8 = implicitly[PlusDefined[() => Option[Int], () => Int]]

    val d3 = implicitly[PlusDefined[Signal[Int], () => Int]]
    val d4 = implicitly[PlusDefined[() => Int, Int]]

    val d5 = implicitly[PlusDefined[Int, () => Option[Int]]]
    val d6 = implicitly[PlusDefined[() => Option[Int], Int]]

    val d7 = implicitly[PlusDefined[() => Int, () => Option[Int]]]
    val d8 = implicitly[PlusDefined[() => Option[Int], () => Int]]

    /*    "Some(1) + 2" should "give Some(3)" in assert(add(some1, 2) == Some(3))
    
        "1.0 + Some(2)" should "give Some(3.0)" in assert(add(1.0, some2) == Some(3.0))
    
        "Some(1L) + Some(2)" should "give Some(3.0)" in assert(add(some1L, some2) == Some(3.0)) */
}
