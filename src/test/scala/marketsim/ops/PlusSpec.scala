package marketsim.ops

import org.scalatest.FlatSpec

class PlusSpec extends FlatSpec
{
    import Conversions._
    import Addable._

    def add[A,B](a : A, b : B)(implicit e : PlusDefined[A,B]) =
    {
        e.plus(a,b)
    }

    "1 + 2" should "give 3" in assert(add(1,2) == 3)
    "1L + 2L" should "give 3L" in assert(add(1L,2L) == 3L)
    "1.0 + 2.0" should "give 3.0" in assert(add(1.0,2.0) == 3.0)

    "1 + 2L" should "give 3L" in assert(add(1, 2L) == 3L)
    "1.0 + 2" should "give 3.0" in assert(add(1.0, 2) == 3.0)

    val some1 = Some(1) : Option[Int]
    val some1L = Some(1L) : Option[Long]
    val some2 = Some(2) : Option[Int]

    "Some(1) + 2.0" should "give Some(3.0)" in assert(add(some1, 2.0) == Some(3.0))

    "1.0 + Some(2)" should "give Some(3.0)" in assert(add(1.0, some2) == Some(3.0))

    "Some(1L) + Some(2)" should "give Some(3.0)" in assert(add(some1L, some2) == Some(3.0))
}
