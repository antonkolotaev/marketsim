package orderbook.linear

object common {

    case class LevelInfo(price : SignedTicks, volumes : List[Quantity])

    def checkResultImpl(side : Side)(mostAggressive : Option[PriceLevel], expected : List[LevelInfo]) : Unit =
    {
        expected match {
            case Nil =>
                assert(mostAggressive.isEmpty || mostAggressive.get.allOrders.isEmpty)
            case LevelInfo(price, volumes) :: tail =>
                assert(mostAggressive.nonEmpty)
                val level = mostAggressive.get
                assert(side == level.side)
                assert(price == level.price)
                val volumeTotal = volumes.sum
                assert(volumeTotal == level.totalVolume)
                val actual = level.ownOrders
                actual forall { o => o.side == side && o.price == price }
                val actualVolumes = (actual map { _.unmatchedVolume }).toList
                assert(volumes == actualVolumes)
                level.getNext foreach { N => assert(level == N.getPrevious.get) }

                checkResultImpl(side)(level.getNext, tail)
        }
    }

}
