package core

import scala.collection.mutable

object Scheduler
{
    type EventId = Int

    class Impl {

        type FutureEvent = (Time, EventId, () => Unit)

        implicit object Ord extends Ordering[FutureEvent] {
            def compare(x: FutureEvent, y: FutureEvent) =
                x._1.x compare y._1.x match {
                    case 0 => -(x._2 compare y._2)
                    case z => -z
                }
        }

        private val future  = new mutable.PriorityQueue[FutureEvent]()
        private var next_id = 0
        private var t       = Time(0)

        def currentTime = t

        def schedule(actionTime : Time, handler : () => Unit)
        {
            if (actionTime < t)
                throw new Exception(s"trying to schedule an event with $actionTime less than current time $t")
            future += ((actionTime, next_id, handler))
            next_id += 1
        }

        def step() = {
            if (future.isEmpty)
                false
            else {
                val (event_time, _, handler) = future.dequeue()
                t = event_time
                handler()
                true
            }
        }

        def fetchAll() = while (step()) {}

        def workTill(limit : Time) =
        {
            var steps = 0
            while (t < limit && step())
                steps += 1
            t = limit
            steps
        }

        def advance(dt : Duration) = workTill(currentTime + dt)
    }

    private val instance = new util.DynamicVariable(Option.empty[Impl])

    def withNew[T](f : Impl => T) : T =
    {
        instance.value match {
            case Some(value) => f(value)
            case None => instance.withValue(Some(new Impl)) { f(instance.value.get) }
        }
    }

    def currentTime = instance.value.get.currentTime

    def schedule(absoluteTimeToAct : Time, whatToDo : => Unit) =
        instance.value.get.schedule(absoluteTimeToAct, () => whatToDo)

    def scheduleAfter(relativeTimeToAct : Duration, whatToDo : => Unit) =
        schedule(currentTime + relativeTimeToAct, whatToDo)

    def async(whatToDo : => Unit) = schedule(currentTime, whatToDo)
}