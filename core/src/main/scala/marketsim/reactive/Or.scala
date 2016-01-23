package marketsim.reactive

import memoization.memo

object Or
{
    @memo
    def apply(a : Signal[Boolean], b : Signal[Boolean]) : Signal[Boolean] =
        new Signal[Boolean](a() && b())
        {
            // we have two inputs: A and B
            val inputs = a :: b :: Nil

            finalConstruct()

            def validate(notifyExternal : Boolean) = {
                updateValue (a(notifyExternal) || b(notifyExternal))
            }

            override def toString() = s"($a || $b)"
        }
}
