package mcts

import game.GameState

import scala.collection.mutable.ListBuffer

/**
  * Created by culim on 2/24/16.
  */
case class GameNode(action : Int = -1, parent : GameNode = null, state : GameState = null) {

    var numberOfWins : Double = 0
    var numberOfVisits : Int = 0
    var children : ListBuffer[GameNode] = ListBuffer.empty
    var untriedActions : Set[Int] = state.getAvailableActions
    var playerIndex : Int = state.getLastPlayerWhoMoved
    val epsilon : Double = 1e-6

    def selectChild : GameNode = {
        val sortedChildren = children.map( node => (node,
            (node.numberOfWins.toDouble/node.numberOfVisits) +
                Math.sqrt(2 * Math.log(numberOfVisits+1) / (node.numberOfVisits+epsilon))
        )).sortBy(_._2)

        return sortedChildren.last._1
    }

    def update(result : Double) : Unit = {
        numberOfVisits += 1;
        numberOfWins += result;
    }

    def addChild(action : Int, state : GameState) : GameNode = {
        val n = new GameNode(action, this, state)
        untriedActions -= action
        children += n

        return n
    }

    override def toString() : String = {
        return  s"[A: $action; " +
                s"W/V: ${numberOfWins}/${numberOfVisits} = ${numberOfWins.toDouble/numberOfVisits}; " +
                s"U: ${untriedActions}"
    }

    def treeToString(indent : Int  ) : String = {
        var s : String = indentString(indent) + this.toString();
        for (c <- children) {
            s += c.treeToString (indent + 1)
        }

        return s
    }

    def indentString(indent : Int) : String = {
        var s = "\n"
        for (i <- 1 to indent) {
            s += "| "
        }

        return s
    }


    def childrenToString() : String = {
        var s = ""
        for (c <- children)  {
            s += c.toString() + "\n"
        }
        return s
    }

}
