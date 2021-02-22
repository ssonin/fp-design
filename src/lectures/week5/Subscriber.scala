package lectures.week5

trait Subscriber {
  def handler(publisher: Publisher)
}
