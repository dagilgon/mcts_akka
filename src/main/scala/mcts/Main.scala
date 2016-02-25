package mcts

import mcts.state.OXOState

/**
  * Created by culim on 2/24/16.
  */
object Main extends App{

    println("Hello, world!")
    var state = new OXOState
    while (state.getAvailableActions.nonEmpty) {

        println(s"Player ${state.totalNumberOfPlayers+1 - state.getLastPlayerWhoMoved}'s turn.")
        println(state.toString)

        var action : Int = -1;
        if (state.getLastPlayerWhoMoved == 1) {
            // Now it is player 2's turn.
            action = UCT.search(state, 100, false)
        }
        else {
            // Now it is player 1's turn.
            action = UCT.search(state, 100, true)
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