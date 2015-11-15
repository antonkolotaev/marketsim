package reactive

abstract class BinaryBase[A,B, Result](a : Signal[A], b : Signal[B], initialValue : Result, label : String)
    extends Signal[Result](initialValue)
{
    protected def F(a : A, b : B) : Result

    if (apply() != initialValue) {
        //println(s"${apply()} != $initialValue")
        assert(apply() == initialValue)
    }

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
    def validate(notifyExternal : Boolean) = {
        //println(s"validate $this")
        // if any input observables changed
        val newA = a(notifyExternal)
        val newB = b(notifyExternal)
        if (cachedA != newA || cachedB != newB) {
            // let's cache them
            cachedA = newA
            cachedB = newB
            // and recalculate our value
            updateValue(F(cachedA, cachedB))
        }
    }

    override def toString() = s"($a $label $b)"
}
