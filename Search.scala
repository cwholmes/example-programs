import scala.collection.immutable.Queue

// prior to scala 2.13 swap out LazyList for Stream

class BfsTraverser[Node](start: Node, f: Node => Queue[Node]) {
  
  def traverse(): LazyList[Node] = {
    def go(q: Queue[Node], visited: Set[Node]): LazyList[Node] = {
      q match {
        case node +: tail =>
          if (visited.contains(node)) go(tail, visited)
          // TODO: should visited always be tracked like this, would a node ever need to be met multiple times
          else node #:: go(tail ++ f(node), visited + node)
        case _ => LazyList()
      }
    }
    go(Queue(start), Set())
  }
  
  def traverseWithDepth(): LazyList[(Node, Int)] = {
    def go(q: Queue[(Node, Int)], visited: Set[Node]): LazyList[(Node, Int)] = {
      q match {
        case (node, depth) +: tail =>
          if (visited.contains(node)) go(tail, visited)
          // TODO: should visited always be tracked like this, would a node ever need to be met multiple times
          else (node, depth) #:: go(tail ++ f(node).iterator.map(x => (x, depth + 1)), visited + node)
        case _ => LazyList()
      }
    }
    go(Queue((start, 0)), Set())
  }
}

class DfsTraverser[Node](start: Node, f: Node => List[Node]) {

  def traverse(): LazyList[Node] = {
    def go(stack: List[Node], visited: Set[Node]): LazyList[Node] = {
      stack match {
        case Nil => LazyList()
        case node :: tail =>
          // the stream will iterate through all possible paths for this node then move to the next
          // TODO: Should this be able to track visited through all nodes or just this path to avoid a loop
          node #:: go(f(node), visited + node) #::: go(tail, visited)
      }
    }
    go(List(start), Set())
  }

  def traverseWithDepth(): LazyList[(Node, Int)] = {
    def go(stack: List[Node], visited: Set[Node], depth: Int): LazyList[(Node, Int)] = {
      stack match {
        case Nil => LazyList()
        case node :: tail =>
          // the stream will iterate through all possible paths for this node then move to the next
          // TODO: Should this be able to track visited through all nodes or just this path to avoid a loop
          (node, depth) #:: go(f(node), visited + node, depth + 1) #::: go(tail, visited, depth)
      }
    }
    go(List(start), Set(), 0)
  }
}
