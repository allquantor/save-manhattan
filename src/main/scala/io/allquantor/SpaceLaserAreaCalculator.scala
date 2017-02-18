package io.allquantor

import io.allquantor.skyline.{Area, Building, calcArea}

import scala.util.Try


object SpaceLaserAreaCalculator {


  def cal2(s:Seq[Building]) : Int = {

    println("Starting calculation")
    val _first = s.head
    println(s"First building: ${_first}")

    val _second = s.tail.head
    println(s"Second building: ${_second}")


    var localArea = calcArea(_first,_second)
    var area = calcArea(_first,_second)
    println(s"Area for first and second and initial local area is:${localArea}")
    var highest = if(_first > _second) _first else _second
    println(s"Highest is set to $highest")
    var secondHighest = if(_first > _second) {
      localArea += calcArea(_second)
      _second
    } else {_first}
    println(s"Second highest is set to $secondHighest")

    val _skyline = s.tail.tail.toIndexedSeq
    println(s"initial skyline is ${_skyline.mkString(",")}")

    for(i <- _skyline.indices) {
      println(s"iteration number ${i}")
      val current = _skyline(i)
      println(s"current building:${current}")
      val previous = Try(_skyline(i-1)).toOption.getOrElse(s.tail.head)
      println(s"previous building:${previous}")
      val next = Try(_skyline(i + 1)).toOption.getOrElse(current)
      println(s"next building:${next}")
      if(current < highest && current <= secondHighest ) {
        println(s"base case")
        println(s"highest $highest")
        println(s"second highest $secondHighest")
        localArea += calcArea(previous,current) + calcArea(current)
        println(s"local area is set to $localArea")
        area += calcArea(previous,current)
        println(s"gloabal area is set to $area")
      } else if(current < highest && current > secondHighest) {
        println("current is bigger than second highest")
        println(s"current:{$current}")
        println(s"second highest:{$secondHighest}")
        area += calcArea(highest,current) - localArea
        println(s"area is set to $area")
        localArea= calcArea(highest,current) - localArea
        println(s"local area is set to $localArea")
        secondHighest = current
      } else if(current >= highest) {
        println(s"current is bigger than highest")
        println(s"current $current")
        println(s"highest $highest")
        println(s"calculate the area between highest and curent ${calcArea(highest,current)}")
        println(s"decreasing the local area ${localArea}")
        area += calcArea(highest,current) - localArea
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




  val dummyBuilding = Building(0,0,0)


  val InitState = (dummyBuilding, Seq[Building](), (0,0))

  def calc(skyline:Seq[Building]) : Int = {

    val first = skyline.head
    val second = skyline.tail.head
    val borderLeft = calcArea(first,second)

    val last = skyline.reverse.head
    val preLast = skyline.reverse.tail.head
    val borderRight = calcArea(preLast,last)


    val res = skyline.foldLeft(InitState) { case ( (highest,states,(localArea, globalArea))  , currentBuilding) =>

      var _area = globalArea
      var _localArea = localArea
      var _highest = highest

      val previousBuilding = states.lastOption.getOrElse(dummyBuilding)



      if(currentBuilding <= previousBuilding) {
        val floodedBetween = calcArea(previousBuilding,currentBuilding)
        _area += floodedBetween
      } else {

        val buildingsInBetween = states.filter(_.between(highest,currentBuilding))

        val betweenTwoLocalHighest = calcArea(highest,currentBuilding)
        val buildingsCoveredWithWater = buildingsInBetween.foldLeft(0){(a,b) => a + calcArea(b)}

        _area += betweenTwoLocalHighest - (buildingsCoveredWithWater + globalArea)

        _highest = if(currentBuilding >= highest) {
          _localArea = 0
          currentBuilding
        } else {
          highest
        }

      }

      (_highest, states :+ currentBuilding, (_localArea,_area))
    }._3._2
    if(skyline.size > 4) {
      res + borderLeft + borderRight
    } else {
      res
    }
  }

}
