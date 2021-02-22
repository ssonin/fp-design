package lectures.week5

class Consolidator(observed: List[BankAccount]) extends Subscriber {

  observed.foreach(_.subscribe(this))
  compute()

  private var total: Int = _

  def totalBalance = total

  private def compute(): Unit = total = observed.map(_.currentBalance).sum

  def handler(publisher: Publisher): Unit = compute()

}
