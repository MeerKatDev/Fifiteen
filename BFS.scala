import scala.collection.mutable.{ Queue, Set }

class BFS(start: Board) extends SearchStrategy {
  solved = if(start.reached) true else search  
  def search: Boolean = {
    val frontier = new Queue[Node]
    val explored = Set[Node]()
    frontier enqueue new Node(start, null, 0)
    def loop: Boolean = {
      if(frontier.isEmpty) return false
      else {
        val node = frontier.dequeue
        explored += node
        node.board.neighbors foreach { b =>
          val child = new Node(b, node, node.moves+1)
          if(!frontier.contains(child) || !explored.contains(child)) {
            if(child.board.reached) {
              lastNode = child
              minMoves = child.moves
              return true
            }
            frontier enqueue child
          }
        }
        loop
      }
    }
    loop
  }
}