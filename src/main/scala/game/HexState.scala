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

            if (row < 10) {
                s += "0"
            }

            s += row.toString
            s += "  "

            for (buffer <- 1 to row) {
                s += " "
            }
            for (col <- 0 to nColumns-1) {
                s += "."
                if (col < nColumns-1) {
                    s += " "
                }
            }
            s += "\n"
        }
        return s
    }
}
