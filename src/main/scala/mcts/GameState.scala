package mcts

/**
  * Created by culim on 2/24/16.
  */
trait GameState {

    def getCopy                         : GameState
    def getLastPlayerWhoMoved           : Int
    def getAvailableActions             : Set[Int]
    def getResult(playerIndex : Int)    : Double
    def doAction(action : Int)          : Unit

}


