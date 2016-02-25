package game

import mcts.GameState

/**
  * Created by culim on 2/24/16.
  */
case class HexState(var nRows : Int, var nColumns : Int) extends GameState{

    var board  : Array[Int] = Array.fill(nRows * nColumns){0}

    override def getCopy: GameState = ???

    override def getAvailableActions: Set[Int] = ???

    override def doAction(action: Int): Unit = ???

    override def getLastPlayerWhoMoved: Int = ???

    override def getResult(playerIndex: Int): Double = ???
}
