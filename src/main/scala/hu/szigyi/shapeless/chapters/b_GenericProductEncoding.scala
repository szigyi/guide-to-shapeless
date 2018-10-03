package hu.szigyi.shapeless.chapters

import shapeless.{HList, ::, HNil}

object Chapter22 extends App {

  val product: String :: Int :: Boolean :: HNil = "Sunday" :: 1 :: false :: HNil

  val first = product.head
  val second = product.tail.head
  val rest = product.tail.tail

  println(first)
  println(second)
  println(rest)

  // compile time error
//  product.tail.tail.tail.head

  val newProduct = 42L :: product

  println(newProduct)
}
