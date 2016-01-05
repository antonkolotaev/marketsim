package marketsim

package object conversions
    extends conversions.ToScalar
    with conversions.ToOption
    with conversions.ToFuncSig
    with conversions.ToUnbound
{
    val a = implicitly[ScalarConversion[Int, Int]].convert(2)
    val b = implicitly[ConversionOpt[Int, Double]].convert(3)
    val c = implicitly[ConversionOpt[Int, Option[Double]]].convert(3)
    val d = implicitly[ConversionOpt[Option[Int], Option[Double]]].convert(Some(3))
    val e = implicitly[ConversionFuncSig[Option[Int], Option[Double]]].convert(Some(3))
    val f = implicitly[ConversionFuncSig[() => Option[Int], () => Option[Double]]].convert(() => Some(3))
    val g = implicitly[ConversionUnbound[() => Option[Int], () => Option[Double]]].convert(() => Some(3))
    val h = implicitly[ConversionUnbound[() => Option[Int], Unbound[() => Option[Double]]]].convert(() => Some(3))
    val i = implicitly[ConversionUnbound[Unbound[() => Option[Int]], Unbound[() => Option[Double]]]].convert(unbound(() => Some(3)))

}
