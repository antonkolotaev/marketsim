package marketsim
package ops

import org.scalatest.FlatSpec

import scala.language.implicitConversions

class MinusSpec extends FlatSpec {

    trait ScalarConversion[-From, +To]
    {
        def convert(from : From) : To
    }

    implicit val int2double = new ScalarConversion[Int, Double]
    {
        def convert(from : Int) : Double = from
    }

    implicit def toOptionId[X] : ScalarConversion[X, Option[X]] =
        new ScalarConversion[X, Option[X]]
        {
            def convert(x : X) : Option[X] = Some(x)
        }

    implicit def toOption[From, To](implicit c : ScalarConversion[From, To]) : ScalarConversion[From, Option[To]] =
        new ScalarConversion[From, Option[To]]
        {
            def convert(x : From) : Option[To] = Some(c convert x)
        }

    implicit def betweenOptions[From, To](implicit c : ScalarConversion[From, To]) : ScalarConversion[Option[From], Option[To]] =
        new ScalarConversion[Option[From], Option[To]]
        {
            def convert(x : Option[From]) : Option[To] = x map c.convert
        }

    val i2d = implicitly[ScalarConversion[Int, Double]]
    val i2od = implicitly[ScalarConversion[Int, Option[Double]]]
    val oi2od = implicitly[ScalarConversion[Option[Int], Option[Double]]]

    def convert[To] = new {
        def apply[From](x : From)(implicit c : ScalarConversion[From, To]) : To = c convert x
    }

    "Int" should "cast to Double" in assertResult(convert[Double](1))(1.0)
    "Int" should "cast to Option[Double]" in assertResult(convert[Option[Double]](1))(Some(1.0))
    "Option[Int]" should "cast to Option[Double]" in assertResult(convert[Option[Double]](Some(1)))(Some(1.0))

    trait HasMinus[T]
    {
        def minus(x : T, y : T) : T
    }

    implicit val intHasMinus = new HasMinus[Int] {
        def minus(x : Int, y : Int) : Int = x - y
    }

    implicit val doubleHasMinus = new HasMinus[Double] {
        def minus(x : Double, y : Double) : Double = x - y
    }

    implicit def optionHasMinus[T](implicit ev : HasMinus[T]) : HasMinus[Option[T]] =
        new HasMinus[Option[T]] {
            def minus(x : Option[T], y : Option[T]) : Option[T] = (x,y) match {
                case (Some(a), Some(b)) => Some(ev.minus(a,b))
                case _ => None
            }
        }

    val intMinus = implicitly[HasMinus[Int]]
    val doubleMinus = implicitly[HasMinus[Double]]
    val optionIntMinus = implicitly[HasMinus[Option[Int]]]
    val optionDoubleMinus = implicitly[HasMinus[Option[Double]]]

    implicit class RichOption[T](x : Option[T])
    {
        def - (y : Option[T])(implicit m : HasMinus[Option[T]]) = m.minus(x,y)
        def - [R](y : R)(implicit m : HasMinus[Option[T]], c : ScalarConversion[R, Option[T]], t : Manifest[R]) = m.minus(x, c convert y)
        def - [R](y : R)(implicit m : HasMinus[R], c : ScalarConversion[Option[T], R]) = m.minus(c convert x, y)
    }

    implicit class RichInt(x : Int)
    {
        def - [R](y : R)(implicit m : HasMinus[R], c : ScalarConversion[Int, R]) = m.minus(c convert x, y)
    }

    trait IntLike[T]
    {
        def mul (x : T, y : Int) : T
    }

    implicit object intIsIntLike extends IntLike[Int]
    {
        def mul (x : Int, y : Int) = x * y
    }

    implicit object optionIntIsIntLike extends IntLike[Option[Int]]
    {
        def mul (x : Option[Int], y : Int) = x map { _ * y }
    }

    trait DoubleLike[T]
    {
        def mul (x : T, y : Double) : T
    }

    implicit object doubleIsDoubleLike extends DoubleLike[Double]
    {
        def mul (x : Double, y : Double) = x * y
    }

    implicit object optionDoubleIsDoubleLike extends DoubleLike[Option[Double]]
    {
        def mul (x : Option[Double], y : Double) = x map { _ * y }
    }

    implicit class RichFunctionInt[T : IntLike](x : () => T)
    {
        def * (y : () => Int) = () => implicitly[IntLike[T]].mul(x(),y())
    }

    implicit class RichFunctionDouble[T : DoubleLike](x : () => T)
    {
        def * (y : () => Double) = () => implicitly[DoubleLike[T]].mul(x(),y())
    }

    implicit class RichScalar[T](x : T)
    {
        def function(implicit c : Conversion[T, () => T]) = c convert x
    }

    implicit class RichFunction[T](x : () => T)
    {
        def some(implicit c : Conversion[() => T, () => Option[T]]) = c convert x
        def unbound(implicit c : Conversion[() => T, Unbound[() => T]]) = c convert x
    }

/*    implicit class RichUnboundFunctionInt[T : IntLike](x : Unbound[() => T])
    {
        def * (y : Int) = {
            (c : Context) =>
                x(c) * y
        }
    } */


/*    "(() => 2) * 12" should "give a function of int" in
        new A_FunctionInt { ensureFunction(A * 12.function, 24, changeAx(3, 36)) }

    "(() => 2).some * 12" should "give a function of Option[Int]" in
        new A_FunctionInt { ensureFunction(A.some * 12.function, Some(24), changeAx(3, Some(36))) }
*/
    /*"(() => Some(2)) * 12" should "give a function of Option[Int]" in
        new A_FunctionInt { ensureFunction(someA * 12.function, Some(24), changeAx(3, Some(36))) } */
    /*

        "(() => 2.0) * 12" should "give a function of Double" in
            new A_FunctionDouble {
                ensureFunction(A * 12.0.function, 24.0, changeAx(3, 36.0))
            }
        "(() => Some(2.0)) * 12" should "give a function of Option[Double]" in
            new A_FunctionDouble { ensureFunction(someA * 12.0.function, Some(24.0), changeAx(3, Some(36.0))) }
    */
    //"Some(5) - Some(2.0)" should "be == Some(3.0)" in assertResult(Some(5) - Some(2))(Some(3))
    //"Some(5) - 2" should "be == Some(3)" in assertResult(Some(5) - 2)(Some(3))
    //"5 - Some(2)" should "be == Some(3)" in assertResult(5 - (Some(2) : Option[Int]))(Some(3))
}
