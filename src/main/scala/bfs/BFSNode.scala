package bfs

/**
  * Created by culim on 2/25/16.
  */
case class BFSNode( id : Int ) {
    override def equals(obj: scala.Any): Boolean = {
        if (obj.isInstanceOf[BFSNode]) {
            var other : BFSNode = obj.asInstanceOf[BFSNode]
            return id == other.id
        }
        return false
    }

    override def hashCode(): Int = {
        return id
    }
}
