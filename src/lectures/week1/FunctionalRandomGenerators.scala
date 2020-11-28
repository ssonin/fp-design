package lectures.week1

import java.util.concurrent.ThreadLocalRandom

object FunctionalRandomGenerators extends App {

  trait Generator[+T] {
    def generate: T

    def map[S](f: T => S): Generator[S] = new Generator[S] {
      def generate = f(Generator.this.generate)
    }

    def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
      def generate = f(Generator.this.generate).generate
    }
  }

  val integers = new Generator[Int] {
    def generate = ThreadLocalRandom.current().nextInt()
  }

  def single[T](x: T): Generator[T] = new Generator[T] {
    def generate = x
  }

  def choose(lo: Int, hi: Int): Generator[Int] = {
    for (x <- integers) yield lo + x % (hi - lo)
  }

  def oneOf[T](xs: T*): Generator[T] = {
    for (idx <- choose(0, xs.length)) yield xs(idx)
  }

  val booleans = for (x <- integers) yield x > 0

  val pairs = for (x <- integers; y <- integers) yield (x, y)

  trait Tree
  case class Leaf(x: Int) extends Tree
  case class Inner(left: Tree, right: Tree) extends Tree

  def trees: Generator[Tree] = for {
    isLeaf <- booleans
    tree <- if (isLeaf) leafs else inners
  } yield tree

  def leafs: Generator[Tree] = for (i <- integers) yield Leaf(i)
  def inners: Generator[Tree] = for {
    left <- trees
    right <- trees
  } yield Inner(left, right)

  println(trees.generate)

}
