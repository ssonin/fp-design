package lectures.week2

object InfiniteSequences extends App {

  def from(n: Int): LazyList[Int] = n #:: from(n + 1)

  val nats = from(0)
  val times4 = nats map (_ * 4)

  println((times4 take 100).toList)

  def sieve(s: LazyList[Int]): LazyList[Int] = {
    s.head #:: sieve(s.tail filter (_ % s.head != 0))
  }

  println((sieve(from(2)) take 100).toList)

}
