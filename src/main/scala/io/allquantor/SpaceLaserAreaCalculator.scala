package io.allquantor

import io.allquantor.skyline.{Area, Building, calcArea}

import scala.util.Try


object SpaceLaserAreaCalculator {
  /**
    *
    * @param previous previous Building in the left
    * @param current current looked Building
    * @return area that could be probably covered with water and should be subtracted later
    */
  def `addToLocalArea in case of overflood`(previous:Building, current:Building): Area = {
    calcArea(previous, current) + calcArea(current)
  }

  /**
    *
    * @param highest the current highest building
    * @param current the current considered building
    * @param localArea the state of the current localArea
    * @return the value should be added to the current area in case when new boundary (highest or second highest) is found.
    */
  def `update Area when new Boundary found`(highest:Building, current: Building, localArea: Area): Area = {
    calcArea(highest, current) - localArea
  }

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
        localArea += `addToLocalArea in case of overflood`(previous, current)
        println(s"local area is set to $localArea")
        area += calcArea(previous, current)
        println(s"gloabal area is set to $area")
      } else if (current < highest && current > secondHighest) {
        println("current is bigger than second highest")
        println(s"current:{$current}")
        println(s"second highest:{$secondHighest}")
        area += `update Area when new Boundary found`(highest, current,localArea)
        println(s"area is set to $area")
        localArea = `addToLocalArea in case of overflood`(highest, current)
        println(s"local area is set to $localArea")
        secondHighest = current
      } else if (current >= highest) {
        println(s"current is bigger than highest")
        println(s"current $current")
        println(s"highest $highest")
        println(s"calculate the area between highest and curent ${calcArea(highest, current)}")
        println(s"decreasing the local area ${localArea}")
        area += `update Area when new Boundary found`(highest, current,localArea)
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
