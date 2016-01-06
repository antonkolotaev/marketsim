package marketsim
package conversions

trait ToScalar {

    implicit def id[T] : ScalarConversion[T,T] = new ScalarConversion[T,T] {
        val convert = (x : T) => x
    }

    implicit object int2long extends ScalarConversion[Int, Long] {
        val convert = (x : Int) => x : Long
    }

    implicit object int2double extends ScalarConversion[Int, Double] {
        val convert = (x : Int) => x : Double
    }

    implicit object long2double extends ScalarConversion[Long, Double] {
        val convert = (x : Long) => x : Double
    }

}
