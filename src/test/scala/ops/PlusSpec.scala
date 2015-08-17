package ops

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
}
