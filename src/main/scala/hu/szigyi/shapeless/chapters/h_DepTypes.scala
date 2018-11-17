package hu.szigyi.shapeless.chapters

import hu.szigyi.shapeless.chapters.Chapter41.Vec
import hu.szigyi.shapeless.chapters.Chapter41._
import hu.szigyi.shapeless.chapters.Chapter42.Second
import shapeless.ops.hlist.{IsHCons, Last}
import shapeless.{::, Generic, HList, HNil}

object Chapter41 {

  //  trait Generic[A] {
  //    type Repr
  //    def to(value: A): Repr
  //    def from(repr: Repr): A
  //  }
  //
  //  trait Generic2[A, Repr] {
  //    def to(value: A): Repr
  //    def from(repr: Repr): A
  //  }

  case class Vec(x: Int, y: Int)

  case class Rect(origin: Vec, size: Vec)

  def getRepr[A](a: A)(implicit gen: Generic[A]) = gen.to(a)

  //  def getRepr2[A, R](value: A)(implicit generic: Generic2[A, R]): R = ???
}

object App41 extends App {
  val vec = Vec(1, 2)
  val reprVec = getRepr(vec)
  //  val reprVec = getRepr2(vec)
  println(reprVec)

  val rectRepr = getRepr(Rect(Vec(0, 0), Vec(5, 5)))
  println(rectRepr)
}

object Chapter42 {

  trait Second[L <: HList] {
    type Out

    def apply(value: L): Out
  }

  object Second {
    type Aux[L <: HList, Out0] = Second[L] {type Out = Out0}

    def apply[L <: HList](implicit second: Second[L]): Aux[L, second.Out] = second

    implicit def hListSecond[A, B, Rest <: HList]: Aux[A :: B :: Rest, B] = new Second[A :: B :: Rest] {
      type Out = B

      def apply(value: A :: B :: Rest): B = value.tail.head
    }
  }

}

object Chapter42App extends App {
  val last1 = Last[String :: Int :: HNil]
  val last2 = Last[Int :: String :: HNil]

  val lastOf1 = last1("foo" :: 123 :: HNil)
  val lastOf2 = last2(456 :: "bar" :: HNil)

  println(lastOf1)
  println(lastOf2)

  //  val lastNil = Last[HNil]
  //  last1(123 :: "hmm" :: HNil)

  val second1 = Second[String :: Boolean :: Int :: HNil]
  val second2 = Second[String :: Int :: Boolean :: HNil]

  val secondOf1 = second1("foo" :: true :: 123 :: HNil)
  val secondOf2 = second2("bar" :: 456 :: false :: HNil)

  println(secondOf1)
  println(secondOf2)

  //  second1("baz" :: HNil)
}

object Chapter43 {
  def lastField[A, Repr <: HList](input: A)(
    implicit gen: Generic.Aux[A, Repr],
    last: Last[Repr]): last.Out = last.apply(gen.to(input))

  case class Wrapper(value: Int)
  case class WrappedMultiple(first: Int, second: String)

  def getWrappedValue[A, Repr <: HList, Head, Tail <: HList](input: A)(
    implicit gen: Generic.Aux[A, Repr],
    isHCons: IsHCons.Aux[Repr, Head, Tail]): Head = gen.to(input).head
}

object App43 extends App {
  import Chapter43._
  val last = lastField(Rect(Vec(1, 2), Vec(3, 4)))
  println(last)

  val wrapped1 = getWrappedValue(Wrapper(42))
  val wrapped2 = getWrappedValue(WrappedMultiple(99, "foo"))
  println(wrapped1)
  println(wrapped2)
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