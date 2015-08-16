package reactive

abstract class BinaryBase[A,B, Result](a : Value[A], b : Value[B], initialValue : Result)
    extends Value[Result](initialValue)
{
    protected def F(a : A, b : B) : Result

    /*if (apply() != initialValue) {
        println(s"${apply()} != $initialValue")
        assert(apply() == initialValue)
    }*/

    /**
     * Last known value of the input observable A
     */
    private var cachedA = a()

    /**
     * Last known value of the input observable B
     */
    private var cachedB = b()

    // we have two inputs: A and B
    val inputs = a :: b :: Nil

    finalConstruct()

    /**
     * Makes the current value of the observable consistent with the inputs
     */
    def validate() = {
        println(s"validate $this")
        // if any input observables changed
        if (cachedA != a() || cachedB != b()) {
            // let's cache them
            cachedA = a()
            cachedB = b()
            // and recalculate our value
            updateValue(F(cachedA, cachedB))
        }
    }
}
