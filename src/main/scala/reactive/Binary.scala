package reactive

/**
 * Computable observable defined by a binary function
 * @param a -- input observable A
 * @param b -- input observable B
 * @param f -- the binary function
 * @tparam A -- value type of the input observable A
 * @tparam B -- value type of the input observable B
 * @tparam Result -- result type of 'f'
 */
case class Binary[A,B, Result](a : Value[A],
                               b : Value[B])
                              (f : (A,B) => Result)
    extends BinaryBase[A,B,Result](a, b, f(a(), b()))
{
    protected def F(a : A, b : B) = f(a,b)
}
