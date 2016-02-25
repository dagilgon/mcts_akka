package mcts.state

import mcts.GameState

/**
  * Created by culim on 2/24/16.
  */
class OXOState extends GameState {

    /**
      * Player indices start from 1
      */
    var lastPlayerWhoMoved = 2
    var totalNumberOfPlayers = 2

    var board : Array[Int] = Array(
        0, 0, 0,    // 0, 1, 2
        0, 0, 0,    // 3, 4, 5
        0, 0, 0     // 6, 7, 8
    )

    override def toString : String = {
        var s = ""
        for ((value,index) <- board.zipWithIndex) {
            s += ".OX"(value)
            if (index % 3 == 2) {
                s += "\n"
            }
        }
        return s
    }

    override def getCopy : GameState = {
        val state = new OXOState()

        state.lastPlayerWhoMoved = this.lastPlayerWhoMoved
        state.board = board.clone()

        return state

    }

    override def getLastPlayerWhoMoved: Int = {
        return lastPlayerWhoMoved
    }

    override def getAvailableActions: Set[Int] = {
        var availableIndices = board.zipWithIndex.filter( x => x._1 == 0)

        if (getPlayerInWinConditions > 0) {
            // Someone has already won, no more actions permitted.
            return Set.empty;
        }
        
        return Set.empty ++ availableIndices.map( x => x._2 )
    }

    /**
      * Perform an action on the board by choosing a grid index
      * to place a mark.
      * @param action the grid index that the player wants to choose
      */
    override def doAction(action: Int): Unit = {
        lastPlayerWhoMoved = (totalNumberOfPlayers+1)-lastPlayerWhoMoved
        board(action) = lastPlayerWhoMoved
    }

    override def getResult(playerIndex: Int): Double = {
        var winningPlayer : Int = getPlayerInWinConditions;
        if (winningPlayer > 0) {
            if (winningPlayer == playerIndex) {
                return 1.0
            }
            else {
                return 0.0
            }
        }
        else {
            // Nobody in win configuration

            if (getAvailableActions.isEmpty) {
                // draw
                return 0.5
            }
            else {
                // draw
                return 0.0
            }
        }


    }

    def getPlayerInWinConditions: Int = {
        var winConfigurations : Set[Tuple3[Int, Int, Int]] = Set(
            (0,1,2),(3,4,5),(6,7,8),    // horizontals
            (0,3,6),(1,4,7),(2,5,8),    // verticals
            (0,4,8),(2,4,6)             // diagonals
        )

        val winningPlayers  = winConfigurations .filter( x => (board(x._1) == board(x._2) && board(x._2) == board(x._3)))
                .map( x => board(x._1))

        if (winningPlayers.nonEmpty) {
            return winningPlayers.head
        }

        return 0;
    }
}
