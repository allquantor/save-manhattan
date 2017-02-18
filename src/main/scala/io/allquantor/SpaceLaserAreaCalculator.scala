package io.allquantor

import io.allquantor.skyline.{Area, Building, calcArea}

import scala.util.Try


object SpaceLaserAreaCalculator {
  /**
    *
    * @param previous previous Building in the left
    * @param current  current looked Building
    * @return area that could be probably covered with water and should be subtracted later
    */
  def `addToLocalArea in case of overflood`(previous: Building, current: Building): Area = {
    calcArea(previous, current) + calcArea(current)
  }

  /**
    *
    * @param highest   the current highest building
    * @param current   the current considered building
    * @param localArea the state of the current localArea
    * @return the value should be added to the current area in case when new boundary (highest or second highest) is found.
    */
  def `update Area when new Boundary found`(highest: Building, current: Building, localArea: Area): Area = {
    calcArea(highest, current) - localArea
  }

  /**
    * Calculate the area of water.
    * @param s Expect an (left -> right) ordered skyline.
    * @return Area
    */

  def calculate(s: Seq[Building]): Area = {

    /**
      *
      * @param first  first left Building.
      * @param second second Building.
      * @return Highest,SecondHighest,LocalArea,GlobalArea
      */
    def initials(first: Building, second: Building): (Building, Building, Area, Area) = {
      // Local Area is the term describing the factor what should be decreased from the global area
      // in case if we have the case of overflooding of a part on the graph.
      var initLocalArea = calcArea(first, second)
      val initArea = calcArea(first, second)

      // Since in the iteration we need to keep track of the current highest
      // and the second highest building. We distinguish here between the first and second.
      val initHighest = if (first > second) first else second

      val initSecondHighest = if (first > second) {
        // In case that the first building is bigger than the second,
        // we have the chance that the second will be flooded.
        // We add the area of the second building to the local area
        // to ensure the subtraction later is correct.
        initLocalArea += calcArea(second)
        second
      } else {
        first
      }
      (initHighest, initSecondHighest, initLocalArea, initArea)
    }

    /**
      *
      * @param s   sequence
      * @param pos current position of the pointer
      * @return (Previous,Current,Next)
      */
    def iterationState(s: IndexedSeq[Building], pos: Int): (Building, Building, Building) = {
      (s(pos), Try(s(pos - 1)).toOption.getOrElse(s.tail.head), Try(s(pos + 1)).toOption.getOrElse(s(pos)))
    }


    s match {
      case Nil => 0
      case _ :: Nil => 0
      case first :: second :: Nil => calcArea(first, second)
      case first :: second :: next :: tail => {
        var (high, sHigh, local, global) = initials(first, second)
        val start = next :: tail toIndexedSeq

        for (i <- start.indices) {
          var (current, previous, next) = iterationState(start, i)

          if (current < high && current <= sHigh) {
            local += `addToLocalArea in case of overflood`(previous, current)
            global += calcArea(previous, current)
          } else if (current < high && current > sHigh) {
            global += `update Area when new Boundary found`(high, current, local)
            local = `addToLocalArea in case of overflood`(high, current)
            sHigh = current
          } else if (current >= high) {
            global += `update Area when new Boundary found`(high, current, local)
            high = current
            sHigh = next
            local = 0
          }
        }
        global
      }
    }
  }

}
