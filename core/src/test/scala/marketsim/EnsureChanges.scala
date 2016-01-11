package marketsim

import marketsim.reactive.Signal
import org.scalamock.scalatest.MockFactory
import org.scalatest.FlatSpec

trait EnsureChanges extends FlatSpec with MockFactory with reactive.CleanMemo
{
    val C = 2

    val ctx = new Context {}

    def ensureSignalOption[T](converted : Signal[Option[T]], initial : T, changes : (() => Unit, T)*) : Unit =
    {
        val handler = mockFunction[Option[T], Unit]("!")
        converted += handler

        assertResult(Some(initial))(converted())
        changes foreach { case (action, ch) =>
            handler expects Some(ch) once ()
            action()
            val actual = converted()
            assertResult(Some(ch))(actual)
        }

        converted -= handler
    }

    def ensureSignal[T](converted : Signal[T], initial : T, changes : (() => Unit, T)*) : Unit =
    {
        val handler = mockFunction[T, Unit]("!")
        converted += handler

        assertResult(initial)(converted())
        changes foreach { case (action, ch) =>
            handler expects ch once ()
            action()
            assertResult(ch)(converted())
        }

        converted -= handler
    }

    def ensureFunctionOption[T](converted : () => Option[T], initial : T, changes : (() => Unit, T)*) : Unit =
    {
        assertResult(Some(initial))(converted())
        changes foreach { case (action, expected) =>
            action()
            val actual = converted()
            assertResult(Some(expected))(actual)
        }
    }

    def ensureFunction[T](converted : () => T, initial : T, changes : (() => Unit, T)*) : Unit =
    {
        assertResult(initial)(converted())
        changes foreach { case (action, ch) =>
            action()
            assertResult(ch)(converted())
        }
    }

    import reactive._

    trait A_FunctionInt
    {
        var V = C
        val A = () => V
        val someA : () => Option[Int] = () => Some(V)
        val unboundA = unbound(A)
        def changeA(x : Int) = (() => V = x, x)
        def changeAx[Y](x : Int, expected : Y) = (() => V = x, expected)
    }

    trait A_FunctionDouble
    {
        var V = 2.0
        val A = () => V
        val someA : () => Option[Double] = () => Some(V)
        val unboundA = unbound(A)
        def changeA(x : Double) = (() => V = x, x)
        def changeAx[Y](x : Double, expected : Y) = (() => V = x, expected)
    }

    import conversions.Implicits._

    trait A_SignalInt
    {
        val a = new Variable(C, "x")
        val A = a : Signal[Int]
        val someA = A.as[Signal[Option[Int]]]
        val unboundA = unbound(A)

        someA += { x => }

        def changeA(x : Int) = (() => a :=! x, x)
        def changeA[T](x : Int, expected : T) = (() => a :=! x, expected)
    }

    val D = 3

    trait B_FunctionInt
    {
        var U = D
        val B = () => U
        val someB = () => Some(U)
        val unboundB = unbound(U)
        def changeB(x : Int) = (() => U = x, x)
    }

    trait B_SignalInt
    {
        val B = new Variable(D, "y")
        val someB = B.as[Signal[Option[Int]]]
        val unboundB = unbound(B)

        someB += { x =>  }

        def changeB(x : Int) = (() => B :=! x, x)
    }

}
