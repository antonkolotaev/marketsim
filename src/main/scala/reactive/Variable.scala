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

    finalConstruct()

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
            tellDependentsToNotifyExternalListeners()
        }
    }
}
