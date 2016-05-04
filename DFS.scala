import scala.collection.mutable.{ ArrayStack, Set }

class DFS(start: Board) extends SearchStrategy {
  solved = if(start.reached) true else search  
  def search: Boolean = {
    val frontier = new ArrayStack[Node]
    val explored = Set[Node]()
    frontier push new Node(start, null, 0)
    def loop: Boolean = {
      if(frontier.isEmpty) return false
      else {
        val node = frontier.pop
        explored += node
        node.board.neighbors foreach { b =>
          val child = new Node(b, node, node.moves+1)
          if(!frontier.contains(child) || !explored.contains(child)) {
            if(child.board.reached) {
              lastNode = child
              minMoves = child.moves
              return true
            }
            frontier push child
          }
        }
        loop
      }
    }
    loop
  }
}