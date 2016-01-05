package marketsim
package conversions

trait ToScalar {

    implicit def id[T] : ScalarConversion[T,T] = new ScalarConversion[T,T] {
        def convert(x : T) = x
    }

    implicit object int2long extends ScalarConversion[Int, Long] {
        def convert(x : Int) = x
    }

    implicit object int2double extends ScalarConversion[Int, Double] {
        def convert(x : Int) = x
    }

    implicit object long2double extends ScalarConversion[Long, Double] {
        def convert(x : Long) = x
    }

}
