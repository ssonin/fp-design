package lectures.week2.waterpouring

class Pouring (capacity: Vector[Int]) {

  type State = Vector[Int]
  val initialState = capacity map (_ => 0)
  val glasses = 0 until capacity.length

  val moves =
    (for (g <- glasses) yield Empty(g)) ++
      (for (g <- glasses) yield Fill(g)) ++
      (for (from <- glasses; to <- glasses if from != to) yield Pour(from, to))

  val initialPath = new Path(Nil, initialState)
  val pathSets = from(Set(initialPath), Set.empty)

  def from(paths: Set[Path], explored: Set[State]): LazyList[Set[Path]] =
    if (paths.isEmpty) LazyList.empty
    else {
      val more = for {
        path <- paths
        next <- moves.map(path.extend)
        if !explored.contains(next.endState)
      } yield next
      paths #:: from(more, explored.concat(more.map(_.endState)))
    }

  def solution(target: Int): LazyList[Path] = {
    for {
      pathSet <- pathSets
      path <- pathSet
      if path.endState.contains(target)
    } yield path
  }

  trait Move {
    def change(state: State): State
  }
  case class Empty(glass: Int) extends Move {
    def change(state: State): State = state.updated(glass, 0)
  }
  case class Fill(glass: Int) extends Move {
    def change(state: State): State = state.updated(glass, capacity(glass))
  }
  case class Pour(from: Int, to: Int) extends Move {
    def change(state: State): State = {
      val amount = (capacity(to) - state(to)).min(state(from))
      state.updated(from, state(from) - amount).updated(to, state(to) + amount)
    }
  }

  class Path(history: List[Move], val endState: State) {
    def extend(move: Move): Path = new Path(move :: history, move.change(endState))
    override def toString = (history.reverse mkString " ") + " --> " + endState
  }
}
