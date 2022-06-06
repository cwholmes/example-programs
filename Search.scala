import scala.collection.immutable.Queue

// prior to scala 2.13 swap out LazyList for Stream

class BfsTraverser[Node](start: Node, f: Node => Queue[Node]) {
  
  def traverse(): Stream[Node] = {
    def go(q: Queue[Node], visited: Set[Node]): LazyList[Node] = {
      if (q.isEmpty) LazyList()
      else {
        val (node, rest) = q.dequeue
        if (visited.contains(node)) go(rest, visited)
        // TODO: should visited always be tracked like this, would a node ever need to be met multiple times
        else node #:: go(rest ++ f(node), visited + node)
      }
    }
    go(Queue(start), Set())
  }
}

class DfsTraverser[Node](start: Node, f: Node => List[Node]) {

  def traverse(): Stream[Node] = {
    def go(stack: List[Node], visited: Set[Node]): LazyList[Node] = {
      if (q.isEmpty) LazyList()
      else {
        stack.foldLeft(Stream()) { (stream, node) =>
          // the stream will iterate through all possible paths for this node then move to the next
          // TODO: Should this be able to track visited through all nodes or just this path to avoid a loop
          stream ++ (node #:: go(f(node), visited + node))
        }
      }
    }
    go(List(start), Set())
  }
}
