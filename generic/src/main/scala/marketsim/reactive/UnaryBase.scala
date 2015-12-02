package marketsim
package reactive

abstract class UnaryBase[A,Result](a : Signal[A], initialValue : Result, label : String)
    extends Signal[Result](initialValue)
{
    if (apply() != initialValue){
        //println(s"${apply()} != $initialValue")
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
    def validate(notifyExternal : Boolean) = {
        val current = a(notifyExternal)
        if (cachedA != current) {
            cachedA = current
            updateValue(F(cachedA))
        }
    }

    override def toString() = s"$label($a)"
}
