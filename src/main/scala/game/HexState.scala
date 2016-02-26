package game

import bfs.{BFS, BFSNode}
import mcts.GameState

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer,Map}

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

    var nodeMap : Map[Int, BFSNode] = Map.empty
    var allNodes : Set[BFSNode] = Set.empty
    var allEdges : Set[Tuple2[BFSNode, BFSNode]] = Set.empty

    buildGraph

    override def getCopy: GameState = {
        val s : HexState = new HexState(nRows, nColumns)
        s.lastPlayerWhoMoved = lastPlayerWhoMoved
        s.totalNumberOfPlayers = totalNumberOfPlayers
        s.board = board.clone()
        s.nodeMap = nodeMap
        s.allEdges = allEdges
        s.allNodes = allNodes

        return s
    }

    def buildGraph = {
        nodeMap = Map.empty
        allNodes = Set.empty
        for (row <- 0 to nRows-1) {
            for (col <- 0 to nColumns-1) {
                var node = new BFSNode(row * nColumns + col)
                allNodes += node
                nodeMap(row * nColumns + col) = node
            }
        }
        allEdges  = Set.empty
        for (row <- 0 to nRows-1) {
            for (col <- 0 to nColumns-1) {

                //  0  1  2  3        |   (0,0) (0,1) (0,2), (0,3)
                //    4  5  6  7      |      (1,0) (1,1) (1,2) (1,3)
                //      8  9  10 11   |         (2,0) (2,1), (2,2) (2,3)

                for ((dy, dx) <- List(
                    (-1, 0),    // up-left
                    (-1, +1),   // up-right
                    (0, -1),    // left
                    (0, 1),     // right
                    (+1, -1),   // down-left
                    (+1, 0)     // down-right
                )) {

                    val currentNode = nodeMap(row * nColumns + col)

                    val neighborCol = col + dx
                    val neighborRow = row + dy

                    if (neighborRow >= 0 && neighborRow < nRows && neighborCol >= 0 && neighborCol < nColumns) {
                        val neighborIndex : Int = neighborRow * nColumns + neighborCol
                        val neighborNode : BFSNode = nodeMap(neighborIndex)
                        allEdges += Tuple2(currentNode, neighborNode)
                        allEdges += Tuple2(neighborNode, currentNode)
                    }
                }

            }
        }
    }

    override def getAvailableActions: Set[Int] = {
        var availableIndices = board.zipWithIndex.filter( x => x._1 == 0)
        val (winner, path) = getPlayerInWinConditions
        if (winner > 0) {
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

    override def getResult(playerIndex: Int): Double = {
        val (winner, path) = getPlayerInWinConditions
        if (winner > 0) {
            // winner found
            if (winner == playerIndex) {
                return 1.0
            }
            else {
                return -1.0
            }
        }
        else {
            if (getAvailableActions.isEmpty) {
                return 0.5
            }
            else {
                return 0.0
            }
        }
    }

    def getPlayerInWinConditions: Tuple2[Int, Array[Int]] = {

        // player 1 goes horizontal (left edge to right edge)
        var leftIndices : ListBuffer[Int] = ListBuffer.empty
        var rightIndices: ListBuffer[Int] = ListBuffer.empty
        for (row <- 0 to nRows-1) {
            leftIndices += row * nColumns
            rightIndices += (row+1) * nColumns -1
        }

        leftIndices = leftIndices.filter(index => board(index) == 1)
        rightIndices = rightIndices.filter(index => board(index) == 1)

        if (leftIndices.nonEmpty && rightIndices.nonEmpty) {
            // possibility of win condition for player 1
            val filteredNodes = allNodes.filter(node => board(node.id) == 1) // player 1
            val filteredEdges = allEdges.filter(pair => filteredNodes.contains(pair._1) && filteredNodes.contains(pair._2))

            for (startIndex <- leftIndices) {
                for (endIndex <- rightIndices) {
//                    println(s"Checking P1 for start=$startIndex end=$endIndex")
                    val path : Array[BFSNode] = BFS.search(filteredNodes, filteredEdges, nodeMap(startIndex), nodeMap(endIndex))
                    if (path.nonEmpty) {
                        return (1, path.map(x => x.id)) // player 1
                    }
                }
            }
        }

        // player 2 goes vertical (top edge to bottom edge)
        var topIndices : ListBuffer[Int] = ListBuffer.empty
        var bottomIndices: ListBuffer[Int] = ListBuffer.empty
        for (col <- 0 to nColumns-1) {
            topIndices += col
            bottomIndices += (nRows-1) * nColumns + col
        }

        topIndices = topIndices.filter(index => board(index) == 2)
        bottomIndices = bottomIndices.filter(index => board(index) == 2)


        if (topIndices.nonEmpty && bottomIndices.nonEmpty) {
            // possibility of win condition for player 1
            val filteredNodes = allNodes.filter(node => board(node.id) == 2) // player 2
            val filteredEdges = allEdges.filter(pair => filteredNodes.contains(pair._1) && filteredNodes.contains(pair._2))

            for (startIndex <- topIndices) {
                for (endIndex <- bottomIndices) {
//                    println(s"Checking P2 for start=$startIndex end=$endIndex")
                    val path : Array[BFSNode] = BFS.search(filteredNodes, filteredEdges, nodeMap(startIndex), nodeMap(endIndex))
                    if (path.nonEmpty) {
                        return (2, path.map(x => x.id)) // player 2
                    }
                }
            }
        }
        return (0, Array.empty);
    }

    override def toString : String = {
        var s = ""

        for (row <- 0 to nRows.toString.length) {
            s += " "
        }

        var (winner, path) = getPlayerInWinConditions

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
                if (path.nonEmpty && board(row*nColumns+col) == winner && path.contains(row*nColumns+col)) {
                    s += "*"
                }
                else {
                    s += ".12"(board(row*nColumns+col))
                }
                if (col < nColumns-1) {
                    s += " "
                }
            }
            s += "\n"
        }
        return s
    }
}
