package hu.szigyi.shapeless.chapters

import shapeless.{:+:, CNil, Coproduct, HNil, Inl, Inr}

object Chapter23 extends App {

  case class Red()
  case class Amber()
  case class Green()

  type Light = Red :+: Amber :+: Green :+: CNil

  val red: Light = Inl(Red())
  val green: Light = Inr(Inr(Inl(Green())))

  println(red)
  println(green)
}

import shapeless.Generic

object Chapter231 extends App {

  sealed trait Shape
  final case class Rectangle(width: Double, height: Double) extends Shape
  final case class Circle(radius: Double) extends Shape

  val rect = Rectangle(3.0, 4.0)
  val circ = Circle(1.0)

  val gen = Generic[Shape]
  val rectRepr = gen.to(rect)
  val circRepr = gen.to(circ)

  println(rectRepr)
  println(circRepr)
}
