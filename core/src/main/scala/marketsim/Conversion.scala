package marketsim

import scala.language.implicitConversions

trait ScalarConversion[-From, To] {
    val convert: From => To
}

trait ConversionOpt[-From, To] {
    val convert: From => To
}

trait ConversionFuncSig[-From, To] {
    val convert : From => To
}

trait ConversionUnbound[-From, To] {
    val convert : From => To
}


