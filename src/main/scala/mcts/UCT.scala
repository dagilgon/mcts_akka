package mcts

import scala.util.Random

/**
  * Created by culim on 2/24/16.
  */
object UCT  {

    def search(rootState : GameState, maxIterations : Int, verbose : Boolean = false) : Int = {
        val rootNode : GameNode = new GameNode(state = rootState)

        var node : GameNode = null;
        var state : GameState = null;
        for (iteration <- 1 to maxIterations) {
            node = rootNode
            state = rootState.getCopy

            // Select
            while (node.untriedActions.isEmpty && node.children.nonEmpty) {
                // Node has exhausted all actions -- fully expanded
                // Node is non-terminal -- still has children to explore
                node = node.selectChild
                state.doAction(node.action)
            }

            // Expand
            if (node.untriedActions.nonEmpty) {
                val action : Int = node.untriedActions.toList(Random.nextInt(node.untriedActions.size))
                state.doAction(action)
                node.addChild(action, state)
            }

            // Rollout
            while (!state.getAvailableActions.isEmpty) {
                state.doAction(state.getAvailableActions.toList(Random.nextInt(state.getAvailableActions.size)))
            }

            // Backpropagate

            while (node != null) {
                node.update(state.getResult(node.playerIndex))
                node = node.parent
            }

        }

        if (verbose) {
            println(rootNode.treeToString(0))
        }
        else {
            println(rootNode.childrenToString());
        }

        return rootNode.children.sortBy(_.numberOfVisits).last.action
    }

}
