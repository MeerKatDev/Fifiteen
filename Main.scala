import java.util.Scanner
import java.io.InputStreamReader

object Main {
  def main(args: Array[String]) = {
    val strategy = args(0)
    print("You chose to solve it with ")
    println (strategy match {
      case "-b" | "--bfs" => "Breadth-first search"
      case "-d" | "--dfs" => "Deep-first search"
      case "-i" | "--idfs" => "Iterative Deep-first search"
      case "-a" | "--a" => "A* search"
      case _ => "Cannot understand your input, please restart and type one strategy (-b, -d, -i, or -a)"
    })
    
    println("Please insert please number of rows and of columns separated by a space")
    val dim = new Scanner(new InputStreamReader(System.in))
    val w = dim.nextInt()
    val k = dim.nextInt()
    println("The board will be made of " + w + " rows and " + k + " columns")
    println("Now please insert the numbers separated by a space")
    val numbers = new Scanner(new InputStreamReader(System.in))
    val grid: List[Int] = (for (i <- 0 until w*k) yield numbers.nextInt()).toList
    
    if(grid.size!=w*k) {
      println("Wrong number of squares inserted")
      System.exit(0)
    }
    
    if(!isSolvable(grid)) {
      println("The puzzle is unsolvable")
      System.exit(0)
    }
    
    val rootNode = new Board(grid, w, k)
    println(rootNode.toString)
    val game: SearchStrategy = strategy match {
      case "-b" | "--bfs" =>  new BFS(rootNode)
      case "-d" | "--dfs" =>  new DFS(rootNode)
      case "-i" | "--idfs" => new IDFS(rootNode)
      case "-a" | "--a" =>    new AStar(rootNode) //new AStar(rootNode, args(1), args(2))
      case _ => null
    }
    if (game.solved) {
      println("Minimum number of moves to the solution: " + game.minMoves)
      val path = (for (e <- game.solution) yield e).toIterator
      while(path.hasNext){
        println(path.next)
      } 
    } else println("Cannot find any solution")
  }
  
  def isSolvable(puzzle: List[Int]): Boolean = {
    var parity = 0
    for(i <- 0 until puzzle.size)
      for(j <- i + 1 until puzzle.size if puzzle(i) != 0)
        if (puzzle(i) > puzzle(j) && puzzle(j) != 0) parity += 1

    if (Math.sqrt(puzzle.size).intValue % 2 == 0)
      return if (puzzle.indexOf(0) % 2 == 0) parity % 2 == 0 else parity % 2 != 0
    else
      return parity % 2 == 0
  }
}

