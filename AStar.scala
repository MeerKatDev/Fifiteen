import java.util.PriorityQueue

class AStar(board: Board, strategyId: String = null, heuristicId: String = null) extends SearchStrategy {
  val pq = new PriorityQueue[Node]{new Node(board,null,0)}
  while (!solved) {    
    val current: Node = pq.poll
    if(current.board.reached){    // if finished go out
      minMoves = current.moves 
      lastNode = current
      solved = true
    }
    current.board.neighbors foreach { b => {
      if(!(current.prevNode != null && current.prevNode.board.equals(b)))   
        pq add new Node(b,current,current.moves+1)        
      }
    }

  }  
}
