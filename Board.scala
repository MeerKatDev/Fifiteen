import scala.collection.mutable.Queue

class Board(val numbers: List[Int], val w: Int, val k: Int) {
  val dim = numbers.length
  val freeIndex = numbers.indexOf(0)
  var reached = true 
  //for((n,i) <- numbers.view.zipWithIndex if(n!=i+1)) reached = false
  numbers.view.zipWithIndex foreach { case (e,i) => if(e != i) reached = false }
  val manhattanDistance =
    (for((n,i) <- numbers.view.zipWithIndex if n>0)
      yield( Math.abs((n-1)/k - i/k)   // corrRow - row
        + Math.abs((n-1)%k-i%k) )      // corrCol - col
    ).sum
  
  def twin: Board = {
    var newArray = numbers.toArray    
    for(i <- 0 until 2 if(!(newArray(i*w)==0 || newArray(i*w+1)==0))){
        val temp = newArray(i*w) 
        newArray(i*w) = newArray(i*w+1)
        newArray(i*w+1) = temp
    }
    return new Board(newArray.toList, w, k)
  }
      
  
  // get zero neighbors
  def neighbors: Iterable[Board] = {
    val kiu: Queue[Board] = new Queue[Board]
    val freePos: Int = numbers.indexOf(0)
    if(freePos%k > 0) // exchange with number to the left
      kiu += new Board(swap(numbers, freePos, freePos-1), w, k)    
    if(freePos%k < k && freePos<dim-1) // exchange with number to the right
      kiu += new Board(swap(numbers, freePos, freePos+1), w, k)    
    if(freePos-k >= 0)  // exchange with number above
      kiu += new Board(swap(numbers, freePos, freePos-k), w, k)    
    if(freePos+k < dim) // exchange with number below
      kiu += new Board(swap(numbers, freePos, freePos+k), w, k)    
    return kiu.toIterable
  }
  
  @Override  
  override def equals(o: Any): Boolean = o match {
    case o: Board => (o.dim == this.dim && o.numbers == this.numbers)
    case _ => false
  }
  
  override def toString: String = {
    val s = new StringBuilder("\n")
    println("---"*k)
    for(i <- 0 until w) {
      for(j <- 0 until k) {
        val n = numbers(i*w+j)
        s append f"$n%3d"
      }
      s append "\n"
    }
    println("---"*k)
    return s.toString
  }
  
  private def swap(l: List[Int], pos0: Int, pos1: Int) = 
    l.updated(pos1,l(pos0)).updated(pos0,l(pos1))
  
}