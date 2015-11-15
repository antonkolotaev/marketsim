package core

import scala.collection.mutable

object Scheduler
{
    type EventId = Int

    class Impl {

        case class FutureEvent(whenToHappen : Time,         // when this event should happen
                               sourceId     : EventId,      // what event caused this event
                               uniqueId     : EventId,      // unique id of the event (do we really need it?)
                               handler      : () => Unit)   // function to call when the event happens

        implicit object Ord extends Ordering[FutureEvent] {
            def compare(x: FutureEvent, y: FutureEvent) =
                x.whenToHappen.x compare y.whenToHappen.x match {
                    case 0 =>
                        x.sourceId compare y.sourceId match {
                            case 0 =>
                                -(x.uniqueId compare y.uniqueId)
                            case z =>
                                -z
                        }
                    case z => -z
                }
        }

        private val future  = new mutable.PriorityQueue[FutureEvent]()
        private var next_id = 0
        private var t       = Time(0)
        private var current_source_id = -1
        private var toCommit = Set.empty[reactive.Variable[_]]

        def currentTime = t

        def schedule(actionTime : Time, handler : () => Unit)
        {
            schedule(actionTime, next_id, handler)
        }

        def scheduleAgain(actionTime : Time, handler : () => Unit)
        {
            schedule(actionTime, current_source_id, handler)
        }

        def schedule(actionTime : Time, id : Int, handler : () => Unit)
        {
            if (actionTime < t)
                throw new Exception(s"trying to schedule an event with $actionTime less than current time $t")
            if (id > next_id)
                throw new Exception(s"calling schedule with event id $id greater than already allocated id $next_id")
            future += FutureEvent(actionTime, id, next_id, handler)
            next_id += 1
        }

        def add(v : reactive.Variable[_]) = {
            toCommit = toCommit + v
        }

        def step() = {
            val e = future.dequeue()
            t = e.whenToHappen
            current_source_id = e.sourceId
            e.handler()
            if (future.isEmpty || future.head.whenToHappen != t || future.head.sourceId != current_source_id){
                runFinalizers()
            }
        }

        private def runFinalizers() = {
            toCommit foreach { _ commit () }
            toCommit = Set.empty[reactive.Variable[_]]
        }

        private def nextActionTime = if (future.isEmpty) Time(Int.MaxValue) else future.head.whenToHappen

        def fetchAll() = while (future.nonEmpty) step()

        def workTill(limit : Time) =
        {
            var steps = 0
            while (nextActionTime <= limit) {
                step()
                steps += 1
            }
            runFinalizers()
            t = limit
            steps
        }

        def advance(dt : Duration) = workTill(currentTime + dt)
    }

    private val instance = new util.DynamicVariable(Option.empty[Impl])

    def withNew[T](f : Impl => T) : T =
    {
        instance.withValue(Some(new Impl)) { f(instance.value.get) }
    }

    // for testing purposes
    def recreate() = {
        val s = new Impl
        instance.value_=(Some(s))
        s
    }

    def currentTime = instance.value.get.currentTime

    def commitAtStepEnd(v : reactive.Variable[_]) =
        instance.value.get add v

    def schedule(absoluteTimeToAct : Time, whatToDo : => Unit) =
        instance.value.get schedule (absoluteTimeToAct, () => whatToDo)

    def scheduleAgain(absoluteTimeToAct : Time, whatToDo : => Unit) =
        instance.value.get scheduleAgain (absoluteTimeToAct, () => whatToDo)

    def after(relativeTimeToAct : Duration)(whatToDo : => Unit) =
        schedule(currentTime + relativeTimeToAct, whatToDo)

    def afterAgain(relativeTimeToAct : Duration)(whatToDo : => Unit) =
        scheduleAgain(currentTime + relativeTimeToAct, whatToDo)

    def async(whatToDo : => Unit) = schedule(currentTime, whatToDo)

    def asyncAgain(whatToDo : => Unit) = scheduleAgain(currentTime, whatToDo)
}