package reactive

/**
 * Computable observable defined by a unary function
 * @param a -- input observable for the unary function
 * @param f -- the unary function
 * @tparam A -- value type of the input observable
 * @tparam Result -- result type of 'f'
 */
case class Unary[A,Result](a : Signal[A], label : String)
                          (f : A => Result)
    extends UnaryBase[A, Result](a, f(a()), label)
{
    protected def F(a : A) = f(a)
}
