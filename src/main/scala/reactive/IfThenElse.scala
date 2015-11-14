package reactive

/**
 * Computable observable for a conditional expression
 * @param condition -- input observable of boolean type for the condition
 * @param ifBranch -- input observable for positive case
 * @param elseBranch -- input observable for negative case
 * @tparam Result -- type of values involved
 */
case class IfThenElse[Result](condition  : Signal[Boolean],
                              ifBranch   : Signal[Result],
                              elseBranch : Signal[Result])
    extends Signal[Result](if (condition()) ifBranch() else elseBranch())
{
    private var cachedCondition = condition()
    private var cachedIf        = ifBranch()
    private var cachedElse      = elseBranch()

    lazy val inputs = condition :: ifBranch :: elseBranch :: Nil

    finalConstruct()

    /**
     * Makes the current value of the observable consistent with the input values
     */
    def validate(notifyExternal : Boolean) = {
        // if condition changed
        val newCondition = condition(notifyExternal)
        if (cachedCondition != newCondition)
            // cache it
            cachedCondition = newCondition

        // if positive branch active and has changed
        if (cachedCondition) {
            val newIf = ifBranch(notifyExternal)
            if (newIf != cachedIf)
            // cache it
                cachedIf = newIf
        }

        // if negative branch active and has changed
        if (!cachedCondition) {
            val newElse = elseBranch(notifyExternal)
            if (newElse != cachedElse)
                // cache it
                cachedElse = elseBranch()
        }

        // update current value and if changed notify all dependent observables and listeners
        updateValue(if (cachedCondition) cachedIf else cachedElse)
    }

    override def toString() = s"if $condition then $ifBranch else $elseBranch"
}
