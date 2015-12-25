package marketsim

import marketsim.reactive.Signal
import org.scalamock.scalatest.MockFactory
import org.scalatest.FlatSpec

trait EnsureChanges extends FlatSpec with MockFactory
{
    val C = 2

    val ctx = new Context {}

    def ensureSignalOptionInt(converted : Signal[Option[Int]], initial : Int, changes : (() => Unit, Int)*) : Unit =
    {
        val handler = mockFunction[Option[Int], Unit]("!")
        converted += handler

        assertResult(Some(initial))(converted())
        changes foreach { case (action, ch) => {
            handler expects Some(ch) once ()
            action()
            val actual = converted()
            assertResult(Some(ch))(actual)
        } }
    }

    def ensureSignalInt(converted : Signal[Int], initial : Int, changes : (() => Unit, Int)*) : Unit =
    {
        val handler = mockFunction[Int, Unit]("!")
        converted += handler

        assertResult(initial)(converted())
        changes foreach { case (action, ch) =>
            handler expects ch once ()
            action()
            assertResult(ch)(converted())
        }
    }

    def ensureFunctionOptionInt(converted : () => Option[Int], initial : Int, changes : (() => Unit, Int)*) : Unit =
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

    def ensureFunctionInt(converted : () => Int, initial : Int, changes : (() => Unit, Int)*) : Unit =
    {
        ensureFunction(converted, initial, changes : _*)
    }

    import reactive._

    def cast[To] = new {
        def apply[From](x : From)(implicit c : Conversion[From, To]) : To = c convert x
    }

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

    trait A_SignalInt
    {
        val A = new Variable(C, "x")
        val someA = cast[Signal[Option[Int]]](A)
        val unboundA = unbound(A)

        someA += { x => }

        def changeA(x : Int) = (() => A :=! x, x)
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
        val someB = cast[Signal[Option[Int]]](B)
        val unboundB = unbound(B)

        someB += { x =>  }

        def changeB(x : Int) = (() => B :=! x, x)
    }

}
