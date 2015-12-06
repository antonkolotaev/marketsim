package marketsim
package ops

import memoization.{memo, GlobalCache}
import org.scalatest.FlatSpec

class MemoSpec extends FlatSpec {

    var volatile = 12

    @memo
    def f(x : Int) = x * volatile

    "memoized function" should "return the same value for the same parameter" in {

        val before = f(1)

        volatile = 9

        val after = f(1)

        assert(before == after)

        println(GlobalCache.Builder)

    }

}

