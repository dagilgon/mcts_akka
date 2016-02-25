package game

import mcts.GameState

/**
  * Created by culim on 2/24/16.
  */
case class HexState(var nRows : Int, var nColumns : Int) extends GameState{

    /**
      * Player indices start from 1
      */
    var lastPlayerWhoMoved = 2
    var totalNumberOfPlayers = 2

    var board  : Array[Int] = Array.fill(nRows * nColumns){0}

    override def getCopy: GameState = {
        val s : HexState = new HexState(nRows, nColumns)
        s.lastPlayerWhoMoved = lastPlayerWhoMoved
        s.totalNumberOfPlayers = totalNumberOfPlayers
        return s
    }

    override def getAvailableActions: Set[Int] = {
        var availableIndices = board.zipWithIndex.filter( x => x._1 == 0)

        if (getPlayerInWinConditions > 0) {
            // Someone has already won, no more actions permitted.
            return Set.empty;
        }

        return Set.empty ++ availableIndices.map( x => x._2 )
    }

    override def doAction(action: Int): Unit = {
        lastPlayerWhoMoved = (totalNumberOfPlayers+1)-lastPlayerWhoMoved
        board(action) = lastPlayerWhoMoved
    }

    override def getLastPlayerWhoMoved: Int = {
        return lastPlayerWhoMoved
    }

    override def getResult(playerIndex: Int): Double = ???

    def getPlayerInWinConditions: Int = {
        return 0;
    }

    override def toString : String = {
        var s = ""

        for (row <- 0 to nRows.toString.length) {
            s += " "
        }

        // -- print the headers (top)
        for (col <- 0 to nColumns-1) {
            s += ("a".charAt(0) + col).toChar
            if (col < nColumns-1) {
                s += " "
            }
        }
        s += "\n"

        // -- print the board
        for (row <- 0 to nRows-1) {

            if (row < 10 && nRows >= 10) {
                s += "0"
            }

            s += row.toString
            s += "  "

            for (buffer <- 1 to row) {
                s += " "
            }
            for (col <- 0 to nColumns-1) {
                s += ".12"(board(row*nColumns+col))
                if (col < nColumns-1) {
                    s += " "
                }
            }
            s += "\n"
        }
        return s
    }
}
