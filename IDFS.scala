class IDFS(val start: Board) extends SearchStrategy { // Iterative-Deepening Search
  var c = 0
  solved = search
  def search: Boolean = {
    while(true && c <1000) {
      println("processing depth " + c)
      val dls = new DLS(start, c) 
      if( dls.result!=2 ) {
        minMoves = dls.minMoves 
        lastNode = dls.lastNode 
        return true
      }
      c += 1
    }
    println("Error: overcame 1000th iteration")
    return false
  }
}

// 0 - solution
// 1 - Failure
// 2 - Cutoff
class DLS(val start: Board, limit: Int) extends SearchStrategy {
  var cutoffOccurred = false
  val result = recursiveDSL(new Node(start, null, 0), limit)
  solved = result == 0
  def recursiveDSL(node: Node, lim: Int): Int = { 
    if(node.board.reached) { lastNode = node; minMoves = node.moves; return 0 }
    else if(lim==0) return 2
    else {
      cutoffOccurred = false
      val it = node.board.neighbors.toIterator 
      while(it.hasNext) {
        val result = recursiveDSL(new Node(it.next,node,node.moves+1), lim-1)
        if(result==2) cutoffOccurred = true
        else if(result!=1) return result
      } 
      return if(cutoffOccurred) 2 else 1
    }
  }
}