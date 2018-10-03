package hu.szigyi.shapeless.chapters

import shapeless.Generic

object Chapter221a extends App {

  case class IceCream(name: String, numCherries: Int, inCone: Boolean)

  val iceCreamGen = Generic[IceCream]

  val iceCream: IceCream = IceCream("Sundae", 1, false)
  val iceCreamRepr = iceCreamGen.to(iceCream)
  val iceCream2: IceCream = iceCreamGen.from(iceCreamRepr)

  case class Employee(name: String, number: Int, manager: Boolean)

  val employee = Generic[Employee].from(iceCreamRepr)

  println(iceCream)
  println(iceCream2)
  println(employee)
  println(iceCreamGen)
  println(iceCreamRepr)
}

object Chapter221b extends App {

  val tuple: (String, Int, Boolean) = ("Hello", 123, true)
  val tupleGen = Generic[(String, Int, Boolean)]
  val tupleRepr = tupleGen.to(tuple)
  val tuple2 = tupleGen.from(tupleRepr)

  println(tuple)
  println(tuple2)
  println(tupleGen)
  println(tupleRepr)
}
