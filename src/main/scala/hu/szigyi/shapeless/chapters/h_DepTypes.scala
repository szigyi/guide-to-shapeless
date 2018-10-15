package hu.szigyi.shapeless.chapters

import hu.szigyi.shapeless.chapters.Chapter41.Vec
import hu.szigyi.shapeless.chapters.Chapter41._
import shapeless.Generic

object Chapter41 {

  //  trait Generic[A] {
  //    type Repr
  //    def to(value: A): Repr
  //    def from(value: Repr): A
  //  }

  case class Vec(x: Int, y: Int)
  case class Rect(origin: Vec, size: Vec)

  def getRepr[A](a: A)(implicit gen: Generic[A]) = gen.to(a)

}

object App41 extends App {
  val reprVec = getRepr(Vec(1, 2))
  println(reprVec)

  val rectRepr = getRepr(Rect(Vec(0, 0), Vec(5, 5)))
  println(rectRepr)
}

object DependentTypeExamples extends App {
  // https://gigiigig.github.io/tlp-step-by-step/introduction.html

  class Outter {
    class Inner
  }

  // Little Dependent type exercises
  val foo1 = new Outter
  val foo2 = new Outter

  val a: Outter#Inner = new foo1.Inner
  val b: Outter#Inner = new foo2.Inner

  val c: foo1.Inner = new foo1.Inner
//  val d: foo2.Bar = new foo1.Bar

  // https://gigiigig.github.io/tlp-step-by-step/abstract-types.html
  trait AbstractMethod {
    def met: String
  }
  trait AbstractType {
    type T
  }
  trait Abstract {
    type T
    def value: T
  }

  object ConcreteString extends Abstract {
    type T = String
    def value: T = "ciao"
  }
  object ConcreteInt extends Abstract {
    type T = Int
    def value: T = 1
  }

  def getValue(a: Abstract): a.T = a.value

  val fs: String = getValue(ConcreteString)
  val fi: Int = getValue(ConcreteInt)
}