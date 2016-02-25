package bfs

import scala.collection.mutable.{ListBuffer, Queue, Map}

/**
  * Created by culim on 2/25/16.
  */
object BFS {
    def search(nodes : Set[BFSNode], edges : Set[Tuple2[BFSNode, BFSNode]], start : BFSNode, end : BFSNode) : Array[BFSNode] =  {

        val parentOf: Map[BFSNode, BFSNode] = Map.empty
        var closed : Set[BFSNode] = Set.empty
        var open : Queue[BFSNode] = Queue.empty

        open += start

        while (open.nonEmpty) {

            val Current : BFSNode = open.dequeue()
            closed += Current

            if (Current.equals(end)) {

                println("Found path")
                var next : BFSNode = Current
                var path : ListBuffer[BFSNode] = ListBuffer.empty
                while (next != null) {
                    path += next
                    if (parentOf.contains(next)) {
                        next = parentOf(next)
                    }
                    else {
                        next = null;
                    }


                }

                return path.reverse.toArray
            }

            for ( (Current, neighbor) <- edges if nodes.contains(neighbor)) {
                if (!closed.contains(neighbor)) {
                    open += neighbor
                    parentOf(neighbor) = Current
                }

            }

        }

        return Array.empty
    }
}
