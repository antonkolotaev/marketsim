package marketsim
package ops

import org.scalatest.FlatSpec
//import com.softwaremill.macmemo.memoize

class MemoSpec extends FlatSpec {

    var volatile = 12

    //@memoize()
    def f(x : Int) = x * volatile

}
