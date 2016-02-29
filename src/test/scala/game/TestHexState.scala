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

    test("P1 and P2 alternate across 5 turns should reduce #available_actions.") {
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

    test("P1 in winning configuration has won should be detected.") {
        var nRows : Int = 5;
        var nColumns : Int = 5;
        var state : HexState = new HexState(nRows, nColumns)

        state.board = Array(
            1, 1, 0, 0, 0,
             0, 1, 1, 0, 0,
              0, 0, 1, 0, 0,
               0, 0, 1, 1, 1,
                0, 0, 2, 2, 2
        )
        state.lastPlayerWhoMoved = 1

        var actual : Int = state.getPlayerInWinConditions._1
        var expected : Int = 1

        assert(expected == actual)
    }

    test("P2 in winning configuration has won should be detected.") {
        var nRows : Int = 5;
        var nColumns : Int = 5;
        var state : HexState = new HexState(nRows, nColumns)

        state.board = Array(
            1, 0, 2, 0, 2,
             0, 2, 1, 2, 0,
              0, 0, 2, 1, 0,
               0, 2, 1, 0, 0,
                2, 1, 1, 1, 0
        )
        state.lastPlayerWhoMoved = 2

        var actual : Int = state.getPlayerInWinConditions._1
        var expected : Int = 2

        assert(expected == actual)
    }

    test("P2 in making final move on 7x7 board should not result in a win.") {
        var nRows : Int = 7;
        var nColumns : Int = 7;
        var state : HexState = new HexState(nRows, nColumns)

        state.board = Array(
            1, 1, 1, 1, 2, 2, 1,
             1, 1, 0, 2, 0, 0, 1,
              1, 2, 2, 1, 0, 0, 0,
               2, 1, 2, 1, 0, 0, 0,
                2, 2, 1, 2, 1, 2, 0,
                 2, 1, 1, 1, 0, 2, 2,
                  1, 1, 0, 2, 1, 2, 2
        )
        state.lastPlayerWhoMoved = 1

        var pair = state.getPlayerInWinConditions
        assert(pair._1 == 0)

        state.doAction(39)

        pair = state.getPlayerInWinConditions

        println(state)
        println(pair._2.mkString(","))
        assert(pair._1 == 0)

    }

}
