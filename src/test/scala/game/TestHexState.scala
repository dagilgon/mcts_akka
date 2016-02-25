package game

import org.scalatest.FunSuite

import scala.util.Random

/**
  * Created by culim on 2/24/16.
  */
class TestHexState extends FunSuite{

    test("A newly created HexState should just have a non-empty board.") {
        var nRows : Int = 11;
        var nColumns : Int = 11;
        val state : HexState = new HexState(nRows, nColumns)

        assert(state.board.length == nRows * nColumns)
    }

    test("A 5 x 5 board where player 1 picks (0,a) should be valid.") {
        var nRows : Int = 5;
        var nColumns : Int = 5;
        val state : HexState = new HexState(nRows, nColumns)

        var gridIndex : Int = 0
        state.doAction(0)

        assert(state.board(0) == 1, "Player 1 should be in position (a,0)")
    }

    test("A 5 x 5 board where player 1 picks (0,a) and player 2 picks (0, b) should be valid.") {
        var nRows : Int = 5;
        var nColumns : Int = 5;
        val state : HexState = new HexState(nRows, nColumns)

        var gridIndex : Int = 0
        state.doAction(0)
        state.doAction(1)

        assert(state.board(0) == 1, "Player 1 should be in position (a,0)")
        assert(state.board(1) == 2, "Player 2 should be in position (a,0)")
    }

    test("A 5 x 5 board where P1 and P2 alternate across 5 turns should reduce #available_actions") {
        var nRows : Int = 5;
        var nColumns : Int = 5;
        val state : HexState = new HexState(nRows, nColumns)


        val nTurns = 5
        for (i <- 1 to nTurns) {
            state.doAction(state.getAvailableActions.toList(Random.nextInt(state.getAvailableActions.size)))
            println(state)
            state.doAction(state.getAvailableActions.toList(Random.nextInt(state.getAvailableActions.size)))
            println(state)

        }


        assert(state.getAvailableActions.size == nRows*nColumns - nTurns*state.totalNumberOfPlayers)



    }

}
