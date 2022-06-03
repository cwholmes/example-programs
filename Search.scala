import scala.collection.immutable.Queue

trait Node

class BfsTraverser(start: Node, f: Node => Queue[Node]) {
  
  def traverse(): Stream[Node] = {
    def go(q: Queue[Node], visited: Set[Node]): Stream[Node] = {
      if (q.isEmpty) collect(Stream())
      else {
        val (node, rest) = q.dequeue
        if (visited.contains(node)) go(rest, visited)
        else node #:: go(rest ++ f(node), visited + node)
      }
    }
    go(Queue(start), Set())
  }
}
