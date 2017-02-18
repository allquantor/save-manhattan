package io.allquantor

package object skyline {

  type Area = Int

  case class Building(xLeft: Int, xRight: Int, y: Int) extends Ordered[Building] {
    override def compare(that: Building): Int = this.y.compare(that.y)
    def after(that:Building):Boolean = this.xLeft > that.xRight
    def before(that:Building):Boolean = that.after(this)
    // associative
    def between(b1:Building,b2:Building):Boolean =
      this.after(b1) && this.before(b2) ||
      this.before(b1) && this.after(b2)
  }

  def calcArea(a: Int, b: Int): Area = a * b

  def calcArea(building: Building): Area =
    calcArea(building.xRight - building.xLeft, building.y)

  // order is important here
  def calcArea(buildingLeft: Building, buildingRight: Building):Area = {
    val minY = Math.min(buildingLeft.y, buildingRight.y)
    calcArea(
      Building(
        buildingLeft.xRight,
        buildingRight.xLeft,
        minY
      )
    )
  }

}
