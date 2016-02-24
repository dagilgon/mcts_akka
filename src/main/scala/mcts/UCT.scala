package mcts

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

            // Expand

            // Rollout

            // Backpropagate

            if (verbose) {
                println(rootNode.treeToString(0))
            }
            else {
                println(rootNode.childrenToString());
            }
        }

        return rootNode.children.sortBy(_.numberOfVisits).last.action
    }

}
