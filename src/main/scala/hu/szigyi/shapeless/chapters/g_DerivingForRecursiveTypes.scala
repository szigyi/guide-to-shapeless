package hu.szigyi.shapeless.chapters

import hu.szigyi.shapeless.chapters.Chapter34.Csv.CsvEncoder
import hu.szigyi.shapeless.chapters.Chapter34.Model.{Branch, Leaf, Tree}
import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr, Lazy}

object Chapter34 {

  object Model {
    sealed trait Tree[A]
    case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
    case class Leaf[A](value: A) extends Tree[A]
  }

  object Csv {
    trait CsvEncoder[A] {
      def encode(value: A): String
    }
    object CsvEncoder {
      def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] = enc
    }
    def writeCsv[A](values: Seq[A])(implicit enc: CsvEncoder[A]): String = {
      values.map(value => enc.encode(value)).mkString("\n")
    }
  }

  object Encode {
    // Debugging implicit resolution -> found out the HList, HNil and CNil encoders are missing!!!
//    val in = CsvEncoder[Int]
//    val br = CsvEncoder[Int :: Int :: HNil]
//    val co = CsvEncoder[Branch[Int] :+: Leaf[Int] :+: CNil]

    implicit val intEncoder: CsvEncoder[Int] = (value: Int) => value.toString
    implicit val hNilEncoder: CsvEncoder[HNil] = (_: HNil) => ""
    implicit val cNilEncoder: CsvEncoder[CNil] = (_: CNil) => throw new RuntimeException("Can not find the coproduct in the AST!")

    implicit def hListEncoder[H, T <: HList](implicit
                                             hEncoder: Lazy[CsvEncoder[H]],
                                             tEncoder: CsvEncoder[T]): CsvEncoder[H :: T] = v => v match {
      case head :: HNil => hEncoder.value.encode(head)
      case head :: tail => hEncoder.value.encode(head) ++ "," ++ tEncoder.encode(tail)
    }
    implicit def coproductEncoder[H, T <: Coproduct](implicit
                                                     hEncoder: Lazy[CsvEncoder[H]],
                                                     tEncoder: CsvEncoder[T]): CsvEncoder[H :+: T] = value => value match {
      case Inl(head) => hEncoder.value.encode(head)
      case Inr(tail) => tEncoder.encode(tail)
    }
    implicit def genericEncoder[A, R](implicit
                                      gen: Generic.Aux[A, R],
                                      enc: Lazy[CsvEncoder[R]]): CsvEncoder[A] = instance => {
      val representation = gen.to(instance)
      enc.value.encode(representation)
    }
  }
}

object App34 extends App {
  import hu.szigyi.shapeless.chapters.Chapter34.Model.{Branch, Leaf, Tree}
  import hu.szigyi.shapeless.chapters.Chapter34.Csv._
  import hu.szigyi.shapeless.chapters.Chapter34.Encode._

  val tree: Seq[Tree[Int]] = Seq(Branch(Branch(Leaf(2), Leaf(3)), Leaf(4)))

  val csv = writeCsv[Tree[Int]](tree)

  println(csv)
}
