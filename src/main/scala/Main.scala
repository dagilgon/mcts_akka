import game.{HexState, OXOState}
import mcts.UCT

import scala.util.Random

/**
  * Created by culim on 2/24/16.
  */
object Main extends App{

    var state = new HexState(7, 7)
    while (state.getAvailableActions.nonEmpty) {

        println(s"Player ${state.totalNumberOfPlayers+1 - state.getLastPlayerWhoMoved}'s turn.")
        println(state.toString)

        var action : Int = -1;
        if (state.getLastPlayerWhoMoved == 1) {
            // Now it is player 2's turn.
            action = UCT.search(state, 200, false)
        }
        else {
            // Now it is player 1's turn.
            action = state.getAvailableActions.toList(Random.nextInt(state.getAvailableActions.size))
        }

        println(s"Player ${state.totalNumberOfPlayers+1 - state.getLastPlayerWhoMoved}'s best action is ${action}")
        println()
        state.doAction(action)

    }

    println(state.toString)

    if (state.getResult(state.lastPlayerWhoMoved) == 1.0) {
        println(s"Aha! Player ${state.lastPlayerWhoMoved} wins!")
    }
    else if (state.getResult(state.lastPlayerWhoMoved) == 0.0) {
        println(s"Hmm, Player ${state.totalNumberOfPlayers+1 - state.lastPlayerWhoMoved} wins!")
    }
    else {
        println(s"It's a draw!")
    }
}
