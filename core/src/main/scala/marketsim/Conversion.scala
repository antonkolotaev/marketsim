package marketsim

import scala.language.implicitConversions

trait ScalarConversion[-From, To] {
    def convert(from: From): To
}

trait ConversionOpt[-From, To] {
    def convert(from: From): To
}

trait ConversionFuncSig[-From, To] {
    def convert(from: From): To
}

trait ConversionUnbound[-From, To] {
    def convert(from: From): To
}


