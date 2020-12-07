package lectures.week3

trait LowPriorityImplicits {
  implicit val intOrdering: Ordering[Int] = Ordering.Int
}


object Implicits extends LowPriorityImplicits {

  implicit val intReverseOrdering: Ordering[Int] = Ordering.Int.reverse

  def main(args: Array[String]): Unit = {
    implicit val n: Int = 42
    def f(implicit x: Int) = x
    println(f)
    println(f(0))

    implicit val world: String = "World"
    def greet(implicit name: String) = s"Hello, $name!"
    println(greet)

    println(List(1, 2, 3).min)

    trait Show[A] {
      def apply(a: A): String
    }
    object Show {
      implicit val showInt: Show[Int] = new Show[Int] {
        def apply(n: Int): String = s"Int($n)"
      }
    }
    println(implicitly[Show[Int]])

    def printValue[A: Show](a: A): Unit = {
      println(implicitly[Show[A]].apply(a))
    }
    printValue(42)


  }

}
