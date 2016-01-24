package marketsim.reactive

import memoization.memo

object Binary {
    /**
     * Computable observable defined by a binary function
     * @param a -- input observable A
     * @param b -- input observable B
     * @param f -- the binary function
     * @tparam A -- value type of the input observable A
     * @tparam B -- value type of the input observable B
     * @tparam Result -- result type of 'f'
     */
    @memo
    def apply[A,B, Result](a : Signal[A],
                           b : Signal[B],
                           label : String)
                          (f : (A,B) => Result) : BinaryBase[A,B,Result] =
        new BinaryBase(a, b, f(a(), b()), label)
    {
        protected def F(a : A, b : B) = f(a,b)
    }
}
