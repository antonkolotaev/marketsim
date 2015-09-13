package ops

import core._
import org.scalamock.scalatest.MockFactory
import org.scalatest.FlatSpec
import reactive.VariableOpt

class DelaySpec extends FlatSpec with MockFactory {

    "Delay" should "merge updates with same time" in {

        Scheduler.withNew { scheduler =>

            val dt = Duration(5)

            val A = new VariableOpt[Int]
            val dA = A delayed dt

            val B = new VariableOpt[Int]
            val dB = B delayed dt

            val result = dA and dB

            val handler = mockFunction[(Option[Int], Option[Int]), Unit]("handler")

            val debut = Scheduler.currentTime

            result += { x =>
                assert(Scheduler.currentTime == debut + dt)
                handler(x)
            }

            A setAndCommit Some(12)
            B setAndCommit Some(34)

            scheduler advance Duration(4)

            handler expects (Some(12), Some(34)) once ()

            scheduler advance Duration(2)
        }

    }


}
