package hu.szigyi.shapeless.chapters

object Chapter21 extends App {

  sealed trait Shape

  final case class Rectangle(width: Double, height: Double) extends Shape
  final case class Circle(radius: Double) extends Shape

  val rect: Shape = Rectangle(3.0, 4.0)
  val circ: Shape = Circle(1.0)

  def area(shape: Shape): Double = shape match {
    case Rectangle(w, h) => w * h
    case Circle(r) => math.Pi * math.pow(r, 2)
  }

  println(area(rect))
  println(area(circ))
}

object Chapter211 extends App {
  type Rectangle = (Double, Double)
  type Circle = Double
  type Shape = Either[Rectangle, Circle]

  val rect: Shape = Left((3.0), (4.0))
  val circ: Shape = Right(1.0)

  def area(shape: Shape): Double = shape match {
    case Left((w, h)) => w * h
    case Right(r) => math.Pi * math.pow(r, 2)
  }

  println(area(rect))
  println(area(circ))
}
