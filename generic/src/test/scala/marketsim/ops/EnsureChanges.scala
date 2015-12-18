package marketsim
package ops

import marketsim.reactive.Signal
import org.scalamock.scalatest.MockFactory
import org.scalatest.FlatSpec

trait EnsureChanges extends FlatSpec with MockFactory
{
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

    def ensureFunctionInt(converted : () => Int, initial : Int, changes : (() => Unit, Int)*) : Unit =
    {
        assertResult(initial)(converted())
        changes foreach { case (action, ch) =>
            action()
            assertResult(ch)(converted())
        }
    }


}
