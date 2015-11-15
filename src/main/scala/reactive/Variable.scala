package reactive

/**
 * Represents a variable whose value can be set externally
 * It doesn't depend on any observables but may have dependent ones
 * @param initialValue -- initial value of the variable
 * @tparam T - type of value held by our observable
 */
class Variable[T](initialValue : T, label : String) extends Signal[T](initialValue)
{
    // variables don't have any inputs
    def inputs = List.empty[Signal[T]]

    finalConstruct()

    /**
     * Variable state is always consistent so we don't need 
     * to provide non-trivial implementation for this method
     */
    protected def validate(notifyExternal : Boolean) = commit()

    /**
     * Sets variable value and causes recalculation of dependent observables
     * @param x -- new value to be set
     */
    def setAndCommit(x : T) = {
        setWithoutCommit(x)
        commit()
    }

    def setWithoutCommit(x : T) = {
        if (updateValue(x))
            invalidate()
    }

    def set(x : T) = {
        setWithoutCommit(x)
        core.Scheduler commitAtStepEnd this
    }

    def commit() = {
        if (dirty || extrenalsAreToBeNotified) {
            dirty = false
            external foreach { _ apply value_ }
            internal foreach { _ notifyExternalListenersIfValueChanged () }
        }
    }

    override def apply() = value_

    def value = value_

    override def toString() = label
}
