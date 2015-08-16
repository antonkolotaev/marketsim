package reactive

abstract class UnaryBase[A,Result](a : Value[A], initialValue : Result)
    extends Value[Result](initialValue)
{
    if (apply() != initialValue){
        println(s"${apply()} != $initialValue")
        assert(apply() == initialValue)
    }

    protected def F(a : A) : Result

    /**
     * Last known value of the input observable
     */
    private var cachedA = a()

    // we have only one input
    val inputs = a :: Nil

    finalConstruct()

    /**
     * Makes the current value of the observable consistent with the input
     */
    def validate() = {
        if (cachedA != a()) {
            cachedA = a()
            updateValue(F(cachedA))
        }
    }
}
