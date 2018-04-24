package home.examples.shapeless

import shapeless._
import org.scalatest.{FlatSpec, Matchers}
import shapeless.Generic.Aux

class ShapelessTest extends FlatSpec with Matchers {

  "a" should "b" in {

    //with ADTs
    sealed trait Shape
    final case class Rectangle(width: Double, height: Double) extends Shape
    final case class Circle(radius: Double) extends Shape
    val rect: Shape = Rectangle(3.0, 4.0)
    val circ: Shape = Circle(1.0)

    def area(shape: Shape): Double =
      shape match {
        case Rectangle(w, h) => w * h
        case Circle(r)       => math.Pi * r * r
      }

    area(rect)

    //with tuples
    type Rectangle2 = (Double, Double)
    type Circle2 = Double
    type Shape2 = Either[Rectangle2, Circle2]
    val rect2: Shape2 = Left((3.0, 4.0))
    val circ2: Shape2 = Right(1.0)

    def area2(shape: Shape2): Double =
      shape match {
        case Left((w, h)) => w * h
        case Right(r)     => math.Pi * r * r
      }

    area2(rect2)

  }

  "HList" should "work like" in {
    val product: String :: Int :: Boolean :: HNil = "Sunday" :: 1 :: false :: HNil

    val product2: Long :: String :: Int :: Boolean :: HNil = 42L :: product

    case class IceCream(name: String, numCherries: Int, inCone: Boolean)

    val iceCreamGen = Generic[IceCream]

    val iceCream = IceCream("Sundae", 1, false)

    val genRep: iceCreamGen.Repr = iceCreamGen.to(iceCream)

    val retrievedIceCream: IceCream = iceCreamGen.from(genRep)

    iceCream shouldBe retrievedIceCream

    case class Employee(name: String, number: Int, manager: Boolean)

    // Create an employee from an ice cream:
    val employee = Generic[Employee].from(Generic[IceCream].to(iceCream))

    employee shouldBe Employee("Sundae", 1, false)
  }

  "Products" should "work like" in {

    val tupleGen = Generic[(String, Int, Boolean)]

    val value: String :: Int :: Boolean :: HNil = tupleGen.to("x", 1, true)

    tupleGen.from("x" :: 1 :: true :: HNil) shouldBe ("x", 1, true)
  }

  "Type class" should "work like" in {

    trait CsvEncoder[A] {
      def encode(value: A): List[String]
    }

    case class Employee(name: String, number: Int, manager: Boolean)

    //define explicit CsvEncoder[Employee] instance
    {
      implicit val employeeEncoder = new CsvEncoder[Employee] {

        override def encode(value: Employee): List[String] =
          List(value.name, value.number.toString, if (value.manager) "yes" else "no")
      }

      def writeCsv[A](values: List[A])(implicit csvEncoder: CsvEncoder[A]): String =
        values
          .map { value ⇒
            csvEncoder.encode(value).mkString(",")
          }
          .mkString("\n")

      val employees = List(Employee("joe", 1, false), Employee("jack", 2, true))

      println(writeCsv(employees))
    }

    //define generic encoder
    {

      //factory for creating CsvEncoder instances
      def createEncoder[A](f: A ⇒ List[String]): CsvEncoder[A] = new CsvEncoder[A] {
        override def encode(value: A): List[String] = f(value)
      }

      implicit val intEncoder: CsvEncoder[Int] = createEncoder(n ⇒ List(n.toString))
      implicit val stringEncoder: CsvEncoder[String] = createEncoder(n ⇒ List(n))
      implicit val booleanEncoder: CsvEncoder[Boolean] =
        createEncoder(bool => List(if (bool) "yes" else "no"))

      implicit val hnilEncoder: CsvEncoder[HNil] = createEncoder(hnil => Nil)

      implicit def hlistEncoder[H, T <: HList](
          implicit
          hEncoder: CsvEncoder[H],
          tEncoder: CsvEncoder[T]): CsvEncoder[H :: T] = createEncoder {
        case h :: t ⇒
          hEncoder.encode(h) ++ tEncoder.encode(t)
      }

      val reprEncoder: CsvEncoder[String :: Int :: Boolean :: HNil] = implicitly

      val result = reprEncoder.encode("abc" :: 123 :: true :: HNil)
      result shouldBe List("abc", "123", "yes")

      implicit val employeeEncoder: CsvEncoder[Employee] = {
        val gen = Generic[Employee]
        val enc = CsvEncoder[gen.Repr]
        createEncoder(empployee ⇒ enc.encode(gen.to(empployee)))
      }
    }

//    implicit val employeeEncoder
  }
}
