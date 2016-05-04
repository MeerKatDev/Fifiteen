abstract class SearchStrategy {  
  var minMoves: Int = 0
  var solved: Boolean = false
  var finished = false
  var lastNode: Node = null
  def solution: Iterable[Board] = {
    if(!solved) return None
    var stack: List[Board] = List()
    var node = lastNode 
    while(node != null){
      stack = node.board :: stack
      node = node.prevNode
    }
    return stack
  }  
  //def search: Boolean
}