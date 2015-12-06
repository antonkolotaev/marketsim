package marketsim
package ops

import marketsim.GlobalCache.Builder
import org.scalatest.FlatSpec
import com.softwaremill.macmemo.memoize
import scala.concurrent.duration._

class MemoSpec extends FlatSpec {

    implicit val cache : com.softwaremill.macmemo.MemoCacheBuilder = new Builder

    var volatile = 12

    @memoize(maxSize = 10, expiresAfter = 1 hour)
    def f(x : Int) = x * volatile

    "memoized function" should "return the same value for the same parameter" in {

        val before = f(1)

        volatile = 9

        val after = f(1)

        assert(before == after)

        //println(cache)

    }
}
