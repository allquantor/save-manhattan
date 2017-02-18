
import io.allquantor.SpaceLaserAreaCalculator
import io.allquantor.skyline._
import org.scalatest._

class CalculationSpec extends FlatSpec with Matchers {


  "Space laser area calculator" should
    "calculate the area of water" in {
    val simpleSkyline = Seq(Building(1, 2, 5), Building(3, 4, 1))
    // val simpleSkylineResult = SpaceLaserAreaCalculator.calculate(simpleSkyline) should be(1)

    // Ensure that reordering breaks the sequence.
    val reversedSkyline = simpleSkyline.reverse
    //    simpleSkylineResult should not be SpaceLaserAreaCalculator.calculate(reversedSkyline)

    // edge cases for empty and singleton
    //SpaceLaserAreaCalculator.calculate(Nil) should be (0)
    //SpaceLaserAreaCalculator.calculate(Seq(Building(1,1,1))) should be (0)

    val smallerFirst = Seq(Building(5, 9, 2), Building(15, 16, 50))
    // (15 - 9) * min(2,50) == 12
    //SpaceLaserAreaCalculator.calculate(smallerFirst) should be(12)

    val triple = simpleSkyline :+ smallerFirst.head
    val tripleResult = (5 - 2) * 2 - calcArea(Building(3, 4, 1))
    //SpaceLaserAreaCalculator.calculate(triple) should be(tripleResult)

    val combined = simpleSkyline ++ smallerFirst
    val combinedAreaResult = ((15 - 2) * 5) - (calcArea(Building(3, 4, 1)) + calcArea(Building(5, 9, 2)))

    SpaceLaserAreaCalculator.cal2(combined) should be(combinedAreaResult)


    val q = Seq(Building(1, 3, 2), Building(4, 5, 1), Building(6, 8, 10), Building(9, 13, 15), Building(15, 17, 2))

    val qres = (calcArea(Building(6, 8, 10), Building(9, 13, 15)) +
      calcArea(Building(1, 3, 2), Building(6, 8, 10)) +
      calcArea(Building(9, 13, 15), Building(15, 17, 2))) -
      calcArea(Building(4, 5, 1))
    SpaceLaserAreaCalculator.cal2(q) should be(qres)


    val qw = Seq(Building(1, 3, 2), Building(4, 5, 1), Building(6, 8, 10), Building(9, 13, 15), Building(15, 17, 2), Building(19, 21, 3))

    // 2 * 3 = +6
    // -1
    // 10 * 1 = +10
    // 3 * 6 = +18
    // -4

    val qwres = calcArea(Building(1, 3, 2), Building(6, 8, 10)) - calcArea(Building(4, 5, 1)) +
      calcArea(Building(6, 8, 10), Building(9, 13, 15)) +
      calcArea(Building(9, 13, 15), Building(19, 21, 3)) -
      calcArea(Building(15, 17, 2))


    SpaceLaserAreaCalculator.cal2(qw) should be(qwres)

  }


}
