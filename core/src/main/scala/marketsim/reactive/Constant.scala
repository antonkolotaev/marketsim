package marketsim.reactive

import memoization.memo

class Constant[T](x : T) extends Signal[T](x)
{
    def inputs = Nil

    def validate(notifyExternal : Boolean) {}

    override def toString() = x.toString
}

object Constant
{
    @memo
    def apply[T](x : T) : Signal[T] = new Constant[T](x)
}
