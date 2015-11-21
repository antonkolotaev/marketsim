package marketsim
package ops

import org.scalatest.FlatSpec

class ConversionSpec extends FlatSpec {

    import Conversions._

    def convert[From, To](x : From)(implicit ev: Conversion[From, To]) = ev.convert(x)

    "Int" should "convert to Double" in { assert(convert[Int, Double](1) == 1.0) }

    "Int" should "convert to Option[Int]" in { assert(convert[Int, Option[Int]](1) == Some(1)) }

    "Int" should "convert to Option[Double]" in { assert(convert[Int, Option[Double]](1) == Some(1.0)) }

    "Option[Int]" should "convert to Option[Double]" in { assert(convert[Option[Int], Option[Double]](Some(1)) == Some(1.0)) }


}
