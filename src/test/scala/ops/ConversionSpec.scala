package ops

import org.scalatest.FlatSpec

class ConversionSpec extends FlatSpec {

    import Conversions._

    def convert[From, To](x : From)(implicit ev: Conversion[From, To]) = ev.convert(x)

    "Int" should "convert to Double" in { assert(convert[Int, Double](1) == 1.0) }

    "Int" should "convert to Option[Int]" in { assert(convert[Int, Option[Int]](1) == Some(1)) }

    "Int" should "convert to Option[Double]" in { assert(convert[Int, Option[Double]](1) == Some(1.0)) }

    "Int" should "convert to Option[Option[Double]]" in { assert(convert[Int, Option[Option[Double]]](1) == Some(Some(1.0))) }
}
