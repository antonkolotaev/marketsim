package marketsim
package ops

class NotSpec extends EnsureChanges {

    import Implicits._

    "!Some(true)" should "be equal Some(false)" in assertResult(!Some(true))(Some(false))
    "!Some(false)" should "be equal Some(true)" in assertResult(!Some(false))(Some(true))
    "!None" should "be equal None" in assertResult(!none[Boolean])(None)

    "!Signal[Boolean]" should "give Signal[Boolean]" in  {

        val A = new reactive.Variable(true, "a")
        val R = !A

        def changeA = change[Boolean, Boolean](A)_

        ensureSignal(R, false,
            changeA(false, true),
            changeA(true, false))

    }

    "!Unbound[Signal[Boolean]]" should "give Unbound[Signal[Boolean]]" in  {

        val A = new reactive.Variable(true, "a")
        val R = !unbound(A)

        def changeA = change[Boolean, Boolean](A)_

        ensureSignal(R(ctx), false,
            changeA(false, true),
            changeA(true, false))

    }

    "!Signal[Option[Boolean]]" should "give Signal[Option[Boolean]]" in  {

        val A = new reactive.Variable(some(true), "a")
        val R = !A

        def changeA = change[Option[Boolean], Option[Boolean]](A)_

        ensureSignal(R, Some(false),
            changeA(Some(false), Some(true)),
            changeA(None, None),
            changeA(Some(true), Some(false)))
    }

    "!Unbound[Signal[Option[Boolean]]]" should "give Unbound[Signal[Option[Boolean]]]" in  {

        val A = new reactive.Variable(some(true), "a")
        val R = !unbound(A)

        def changeA = change[Option[Boolean], Option[Boolean]](A)_

        ensureSignal(R(ctx), Some(false),
            changeA(Some(false), Some(true)),
            changeA(None, None),
            changeA(Some(true), Some(false)))
    }

    "!() => Boolean" should "give () => Boolean" in  {

        val a = new reactive.Variable(true, "a")
        val A = a : () => Boolean
        val R = !A

        def changeA = change[Boolean, Boolean](a)_

        ensureFunction(R, false,
            changeA(false, true),
            changeA(true, false))

    }

    "!Unbound[() => Boolean]" should "give Unbound[() => Boolean]" in  {

        val a = new reactive.Variable(true, "a")
        val A = a : () => Boolean
        val R = !unbound(A)

        def changeA = change[Boolean, Boolean](a)_

        ensureFunction(R(ctx), false,
            changeA(false, true),
            changeA(true, false))

    }

    "!() => Option[Boolean]" should "give () => Option[Boolean]" in  {

        val a = new reactive.Variable(some(true), "a")
        val A = a : () => Option[Boolean]
        val R = !A

        def changeA = change[Option[Boolean], Option[Boolean]](a)_

        ensureFunction(R, some(false),
            changeA(some(false), some(true)),
            changeA(None, None),
            changeA(some(true), some(false)))

    }

    "!Unbound[() => Option[Boolean]]" should "give Unbound[() => Option[Boolean]]" in  {

        val a = new reactive.Variable(some(true), "a")
        val A = a : () => Option[Boolean]
        val R = !unbound(A)

        def changeA = change[Option[Boolean], Option[Boolean]](a)_

        ensureFunction(R(ctx), some(false),
            changeA(some(false), some(true)),
            changeA(None, None),
            changeA(some(true), some(false)))

    }
}
