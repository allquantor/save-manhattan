package io.allquantor

import io.allquantor.skyline.{Area, Building, calcArea}

import scala.util.Try


object SpaceLaserAreaCalculator {

  /*
  def calculate(s: Seq[Building]): Area = {
    s match {
      case Nil => 0
      case _ :: Nil => 0
      case first :: second :: Nil => calcArea(first, second)
      case first :: second :: next :: tail => {

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

        // since we already have the calc for first and second
        // we not need to repeat it.
        (next :: tail).foldLeft(
          // building triple
          (first, second, next,
            // state of the highest and second-highest
            initHighest, initSecondHighest,
            // local and global areas
            initLocalArea, initArea)
        ) { case ((_first, _second, _next,
        highest, secondHighest,
        localArea, globalArea),
        currentBuilding) =>
            if(currentBuilding < highest &&
              currentBuilding <= secondHighest) {

              val area = calcArea(_second, currentBuilding)

              (_second,_next,_,
                highest,secondHighest,
                localArea + area + calcArea(currentBuilding),
                globalArea + area
              )
            }
          1


        }

        //


      }

    }
  }
  */

  def cal2(s: Seq[Building]): Int = {

    println("Starting calculation")
    val _first = s.head
    println(s"First building: ${_first}")

    val _second = s.tail.head
    println(s"Second building: ${_second}")

    var localArea = calcArea(_first, _second)
    var area = calcArea(_first, _second)
    println(s"initial area is:${area}")
    var highest = if (_first > _second) _first else _second
    println(s"Highest is set to $highest")
    var secondHighest = if (_first > _second) {
      localArea += calcArea(_second)
      _second
    } else {
      _first
    }
    println(s"initial local area is:${localArea}")
    println(s"Second highest is set to $secondHighest")

    val _skyline = s.tail.tail.toIndexedSeq
    println(s"initial skyline is ${_skyline.mkString(",")}")

    for (i <- _skyline.indices) {
      println(s"iteration number ${i}")
      val current = _skyline(i)
      println(s"current building:${current}")
      val previous = Try(_skyline(i - 1)).toOption.getOrElse(s.tail.head)
      println(s"previous building:${previous}")
      val next = Try(_skyline(i + 1)).toOption.getOrElse(current)
      println(s"next building:${next}")
      if (current < highest && current <= secondHighest) {
        println(s"base case")
        println(s"highest $highest")
        println(s"second highest $secondHighest")
        localArea += calcArea(previous, current) + calcArea(current)
        println(s"local area is set to $localArea")
        area += calcArea(previous, current)
        println(s"gloabal area is set to $area")
      } else if (current < highest && current > secondHighest) {
        println("current is bigger than second highest")
        println(s"current:{$current}")
        println(s"second highest:{$secondHighest}")
        area += calcArea(highest, current) - localArea
        println(s"area is set to $area")
        localArea = calcArea(highest, current) + calcArea(current)
        println(s"local area is set to $localArea")
        secondHighest = current
      } else if (current >= highest) {
        println(s"current is bigger than highest")
        println(s"current $current")
        println(s"highest $highest")
        println(s"calculate the area between highest and curent ${calcArea(highest, current)}")
        println(s"decreasing the local area ${localArea}")
        area += calcArea(highest, current) - localArea
        println(s"area is set to $area")
        highest = current
        println(s"new highest is set to ${highest}")
        secondHighest = next
        println(s"new second highest is set to ${secondHighest}")
        localArea = 0
      } else {
        throw new Exception("This should not happen!")
      }
    }
    area
  }

}
