package hu.szigyi.shapeless.chapters

import hu.szigyi.shapeless.chapters.Chapter322.Model.IceCream
import shapeless.{Generic, HList, HNil}

object Chapter322 {

  object Model {
    case class Employee(name: String, number: Int, manager: Boolean)
    case class IceCream(name: String, numCherries: Int, inCone: Boolean)
  }

  object Csv {
    import CsvEncoder._
    implicit val stringEncoder: CsvEncoder[String] = instance(str => List(str))
    implicit val intEncoder: CsvEncoder[Int] = instance(i => List(i.toString))
    implicit val booleanEncoder: CsvEncoder[Boolean] = instance(b => if (b) List("yes") else List("no"))
    implicit val hNilEncoder: CsvEncoder[HNil] = instance(_ => Nil)
    implicit def hListEncoder[H, T <: HList](implicit hEncoder: CsvEncoder[H], tEncoder: CsvEncoder[T]): CsvEncoder[H :: T] = instance {
      case h :: t => hEncoder.encode(h) ++ tEncoder.encode(t)
    }

    //    implicit val iceCreamEncoder: CsvEncoder[IceCream] = {
    //      val gen: Aux[IceCream, String :: Int :: Boolean :: HNil] = Generic[IceCream]
    //      val enc: CsvEncoder[String :: Int :: Boolean :: HNil] = CsvEncoder[gen.Repr]
    ////                                                            No implicit found for parameter enc: CsvEncoder[gen.Repr] - (?: CsvEncoder[gen.Repr]
    //      instance(iceCream => enc.encode(gen.to(iceCream)))
    //    }
    implicit def genericEncoder[A, R](implicit
                                      gen: Generic[A] {type Repr = R},
                                      enc: CsvEncoder[R]): CsvEncoder[A] = {
      instance((a: A) => {
        val repr: R = gen.to(a)
        enc.encode(repr)
      })
    }

    implicit def writeCsv[A](values: List[A])(implicit enc: CsvEncoder[A]): String =
      values.map(value => enc.encode(value).mkString(",")).mkString("\n")

    trait CsvEncoder[A] {
      def encode(value: A): List[String]
    }
    object CsvEncoder {
      def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] = enc
      def instance[A](func: A => List[String]): CsvEncoder[A] = (value: A) => func(value)
    }
  }
}

object App322 extends App {
  import hu.szigyi.shapeless.chapters.Chapter322.Csv._
  val iceCreams: List[IceCream] = List(
    IceCream("Sundae", 1, false),
    IceCream("Cornetto", 0, true),
    IceCream("Banana Split", 0, false)
  )

  val iceCreamsCsv: String = writeCsv[IceCream](iceCreams)

  println(iceCreamsCsv)
}
