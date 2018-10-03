package hu.szigyi.shapeless.chapters

import hu.szigyi.shapeless.chapters.Chapter31.Csv._
import hu.szigyi.shapeless.chapters.Chapter31.Model.{Employee, IceCream}
import shapeless.Generic

object Chapter31 {

  object Csv {
    implicit val employeeEncoder: CsvEncoder[Employee] = (e: Employee) => Seq(e.name, e.number.toString, if (e.manager) "yes" else "no")
    implicit val iceCreamEncoder: CsvEncoder[IceCream] = (i: IceCream) => Seq(i.name, i.numCherries.toString, if (i.inCone) "yes" else "no")

    import CsvEncoder._
    implicit val booleanEncoder: CsvEncoder[Boolean] = instance(b => if (b) Seq("yes") else Seq("no"))

    implicit def writeCsv[A](values: Seq[A])(implicit enc: CsvEncoder[A]): String =
      values.map(value => enc.encode(value).mkString(",")).mkString("\n")

    implicit def pairEncoder[A, B](implicit aEncoder: CsvEncoder[A], bEncoder: CsvEncoder[B]): CsvEncoder[(A, B)] = {
      (pair: (A, B)) => {
        val (a, b) = pair
        aEncoder.encode(a) ++ bEncoder.encode(b)
      }
    }

    trait CsvEncoder[A] {
      def encode(value: A): Seq[String]
    }
    object CsvEncoder {
      /**
        * Summoner, materializer
        *
        * @param enc
        * @tparam A
        * @return
        */
      def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] = enc

      /**
        * Constructor
        *
        * @param func
        * @tparam A
        * @return
        */
      def instance[A](func: A => Seq[String]): CsvEncoder[A] = (value: A) => func(value)
    }
  }

  object Model {
    case class Employee(name: String, number: Int, manager: Boolean)
    case class IceCream(name: String, numCherries: Int, inCone: Boolean)
  }
}

object App31 extends App {
  val employees: Seq[Employee] = Seq(
    Employee("Bill", 1, true),
    Employee("Peter", 2, false),
    Employee("Milton", 3, false)
  )
  val iceCreams: Seq[IceCream] = Seq(
    IceCream("Sundae", 1, false),
    IceCream("Cornetto", 0, true),
    IceCream("Banana Split", 0, false)
  )

  val employeesCsv: String = writeCsv(employees)
  //                         writeCsv[Employee](employees)(employeeEncoder)
  val iceCreamsCsv: String = writeCsv(iceCreams)
  //                         writeCsv[IceCream](iceCreams)(iceCreamEncoder)

  println(employeesCsv)
  println(iceCreamsCsv)

  println("Pair Encoder:")
  val employeeAndIceCream = writeCsv(employees zip iceCreams)
//                          writeCsv(employees zip iceCreams)(pairEncoder(employeeEncoder, iceCreamEncoder))

  println(employeeAndIceCream)
}

import shapeless.{HNil, ::, HList}

object Chapter321 {

  object Csv {
    import CsvEncoder._
    implicit val stringEncoder: CsvEncoder[String] = instance(str => Seq(str))
    implicit val intEncoder: CsvEncoder[Int] = instance(i => Seq(i.toString))
    implicit val booleanEncoder: CsvEncoder[Boolean] = instance(b => if (b) Seq("yes") else Seq("no"))
    implicit val hNilEncoder: CsvEncoder[HNil] = instance(_ => Nil)

    implicit def hListEncoder[H, T <: HList](implicit hEncoder: CsvEncoder[H], tEncoder: CsvEncoder[T]): CsvEncoder[H :: T] = instance {
      case h :: t => hEncoder.encode(h) ++ tEncoder.encode(t)
    }

    implicit def writeCsv[A](values: Seq[A])(implicit enc: CsvEncoder[A]): String =
      values.map(value => enc.encode(value).mkString(",")).mkString("\n")

    trait CsvEncoder[A] {
      def encode(value: A): Seq[String]
    }

    object CsvEncoder {
      def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] = enc

      def instance[A](func: A => Seq[String]): CsvEncoder[A] = (value: A) => func(value)
    }
  }
}

object App321 extends App {
  import Chapter321.Csv._
  val reprEncoder: CsvEncoder[String :: Int :: Boolean :: HNil] = implicitly

  val encoded: Seq[String] = reprEncoder.encode("abc" :: 123 :: true :: HNil)

  println(encoded)
}
