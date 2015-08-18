package reactive

/**
 * Represents a variable whose value can be set externally
 * It doesn't depend on any observables but may have dependent ones
 * @param initialValue -- initial value of the variable
 * @tparam T - type of value held by our observable
 */
class Variable[T](initialValue : T) extends Value[T](initialValue)
{
    // variables don't have any inputs
    val inputs = Nil

    private val dependent = collection.mutable.Set.empty[Value[_]]

    finalConstruct()

    protected override def registerInRoot(what : Value[_], visited : collection.mutable.Set[Value[_]]) = {
        if (this != what)
            dependent += what
    }

    protected override def removeFromRoot(what : Value[_], visited : collection.mutable.Set[Value[_]]) = {
        if (this != what)
            dependent -= what
    }

    /**
     * Variable state is always consistent so we don't need 
     * to provide non-trivial implementation for this method
     */
    protected def validate() {}

    /**
     * Sets variable value and causes recalculation of dependent observables
     * @param x -- new value to be set
     */
    def set(x : T) = {
        if (updateValue(x)) //
        {
            invalidate()
            external foreach { _ apply x }
            internal foreach { _ notifyExternalListenersIfValueChanged () }
        }
    }

    override def apply() = value_

}
