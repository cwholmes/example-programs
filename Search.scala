import scala.collection.immutable.Queue

trait Node

class BfsTraverser(start: Node, f: Node => Queue[Node]) {
  
  def traverse(): Stream[Node] = {
    def go(q: Queue[Node], visited: Set[Node]): Stream[Node] = {
      if (q.isEmpty) Stream()
      else {
        val (node, rest) = q.dequeue
        if (visited.contains(node)) go(rest, visited)
        else node #:: go(rest ++ f(node), visited + node)
      }
    }
    go(Queue(start), Set())
  }
}

class DfsTraverser(start: Node, f: Node => List[Node]) {

  def traverse(): Stream[Node] = {
    def go(stack: List[Node], visited: Set[Node]): Stream[Node] = {
      if (q.isEmpty) Stream()
      else {
        stack.foldLeft(Stream()) { (stream, node) =>
          // the stream will iterate through all possible paths for this node then move to the next
          stream ++ (node #:: go(f(node), visited + node))
        }
      }
    }
    go(List(start), Set())
  }
}
