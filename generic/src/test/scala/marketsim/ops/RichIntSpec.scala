package marketsim
package ops

import marketsim.ops.overloads._
import org.scalatest.FlatSpec

class RichIntSpec extends FlatSpec
{
    "1 + Some(2)" should "give Some(3)" in assert(1 + Some(2) == Some(3))

    "1 + Var(2)" should "give Binary" in assert((1 + reactive.Constant(2))() ==3)
    "1 + Var(Some(2))" should "give Binary" in assert((1 + reactive.Constant(Some(2))).apply() == Some(3))

    "1 + (() => 2)" should "give () => 3" in assert((1 + (() => 2))() ==3)
    "1 + (() => Some(2))" should "give () => Some(3)" in assert((1 + (() => Some(2))).apply() == Some(3))

    "Some(1) - 2" should "give Some(-1)" in assert(Some(1) - 2 == Some(-1))
}
