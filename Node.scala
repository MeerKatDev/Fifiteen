class Node(val board: Board, val prevNode: Node, val moves: Int) extends Comparable[Node]{
  def compareTo(that: Node): Int = {
    val thisPriority = this.moves + this.board.manhattanDistance
    val thatPriority = that.moves + that.board.manhattanDistance
    return (if (thisPriority < thatPriority) -1 else (if (thisPriority > thatPriority) 1 else 0))
  }
}
