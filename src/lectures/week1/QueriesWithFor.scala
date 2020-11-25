package lectures.week1

object QueriesWithFor extends App {

  def isPrime(n: Int): Boolean = !(2 +: (3 to Math.sqrt(n).toInt by 2) exists (n % _ == 0))

  case class Book(title: String, authors: List[String])

  val books = Set(
    Book(title = "Structure and Interpretation of Computer Programs",
      authors = List("Abelson, Harald", "Sussman, Gerald J.")),
    Book(title = "Introduction to Functional Programming",
      authors = List("Bird, Richard", "Wadler, Phil")),
    Book(title = "Effective Java",
      authors = List("Bloch, Joshua")),
    Book(title = "Java Puzzlers",
      authors = List("Bloch, Joshua", "Gafter, Neal")),
    Book(title = "Programming in Scala",
      authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Bill")),
    Book(title = "Test", authors = List("Bloch, Joshua")))


  val q1 = for (b <- books; a <- b.authors if a startsWith "Bird,") yield b.title
  val q1_ = for (b <- books; _ <- b.authors.withFilter(_ startsWith "Bird,")) yield b.title
  val q1__ = books.flatMap(b => for (_ <- b.authors.withFilter(_ startsWith "Bird,")) yield b.title)
  val q1___ = books.flatMap(b => b.authors.withFilter(_ startsWith "Bird,").map(_ => b.title))
  println(q1___)


  val n = 42
  val lst = (1 until n).flatMap(i => (1 until i).withFilter(j => isPrime(i + j)).map(j => (i, j)))
  println(lst)


  val authorsWithMoreThanOneBook = for {
    b1 <- books
    b2 <- books
    if b1.title < b2.title
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
  } yield a1

  println(authorsWithMoreThanOneBook)
}
