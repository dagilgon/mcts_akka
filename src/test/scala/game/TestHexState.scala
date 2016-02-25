package game

import org.scalatest.FunSuite

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

}
