package marketsim
package ops

import memoization.memo

object Not {

    val not = (x : Boolean) => !x
    val notOpt = (x : Option[Boolean]) => x map { y => !y }

    trait Implicits {

        implicit class OptionBool(x : Option[Boolean])
        {
            def `unary_!`() = x map { y => !y }
        }

        implicit class SignalBool(x : reactive.Signal[Boolean])
        {
            def `unary_!`() = reactive.Unary(x, "not")(not)
        }

        implicit class SignalOptionBool(x : reactive.Signal[Option[Boolean]])
        {
            def `unary_!`() = reactive.Unary(x, "not")(notOpt)
        }

        implicit class FunctionBool(x : () => Boolean)
        {
            def `unary_!`() = CachedFunction(Compose(x, not))
        }

        implicit class FunctionOptionBool(x : () => Option[Boolean])
        {
            def `unary_!`() = CachedFunction(Compose(x, notOpt))
        }

        @memo
        def UnboundOptionBoolImpl(x : Unbound[Option[Boolean]]) : Unbound[Option[Boolean]] =
        {
            (ctx : Context) => x(ctx) map { y => !y }
        }

        implicit class UnboundOptionBool(x : Unbound[Option[Boolean]])
        {
            def `unary_!`() = UnboundOptionBoolImpl(x)
        }

        @memo
        def UnboundSignalBoolImpl(x : Unbound[reactive.Signal[Boolean]]) : Unbound[reactive.Signal[Boolean]] =
        {
            (ctx : Context) => reactive.Unary(x(ctx), "not")(not)
        }

        implicit class UnboundSignalBool(x : Unbound[reactive.Signal[Boolean]])
        {
            def `unary_!`() = UnboundSignalBoolImpl(x)
        }

        @memo
        def UnboundSignalOptionBoolImpl(x : Unbound[reactive.Signal[Option[Boolean]]])
            : Unbound[reactive.Signal[Option[Boolean]]] =
        {
            (ctx : Context) => reactive.Unary(x(ctx), "not")(notOpt)
        }

        implicit class UnboundSignalOptionBool(x : Unbound[reactive.Signal[Option[Boolean]]])
        {
            def `unary_!`() = UnboundSignalOptionBoolImpl(x)
        }

        @memo
        def UnboundFunctionBoolImpl(x : Unbound[() => Boolean]) : Unbound[() => Boolean] =
        {
            (ctx : Context) => CachedFunction(Compose(x(ctx), not))
        }

        implicit class UnboundFunctionBool(x : Unbound[() => Boolean])
        {
            def `unary_!`() = UnboundFunctionBoolImpl(x)
        }

        @memo
        def UnboundFunctionOptionBoolImpl(x : Unbound[() => Option[Boolean]]) : Unbound[() => Option[Boolean]] =
        {
            (ctx : Context) => CachedFunction(Compose(x(ctx), notOpt))
        }

        implicit class UnboundFunctionOptionBool(x : Unbound[() => Option[Boolean]])
        {
            def `unary_!`() = UnboundFunctionOptionBoolImpl(x)
        }

    }

}
