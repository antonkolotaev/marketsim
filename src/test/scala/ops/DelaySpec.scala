package ops

import core._
import org.scalamock.scalatest.MockFactory
import org.scalatest.FlatSpec
import reactive.VariableOpt

class DelaySpec extends FlatSpec with MockFactory {

    "Delay" should "merge updates with same time" in {

        Scheduler.withNew { scheduler =>

            val dt = Duration(5)

            val A = new VariableOpt[Int]("A")
            val dA = A delayed dt

            val B = new VariableOpt[Int]("B")
            val dB = B delayed dt

            val result = dA and dB

            val handler = mockFunction[(Option[Int], Option[Int]), Unit]("handler")

            val debut = Scheduler.currentTime

            result += { x =>
                assert(Scheduler.currentTime == debut + dt)
                handler(x)
            }

            Scheduler schedule (Time(0), {
                A set Some(12)
                B set Some(34)
            })

            scheduler advance Duration(4)

            handler expects (Some(12), Some(34)) once ()

            scheduler advance Duration(2)
        }

    }


}
