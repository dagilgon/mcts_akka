package mcts_distributed

import akka.actor.{ActorRef, ActorSystem}
import mcts_distributed.Node._

import scala.util.Random
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import game.GameState

import scala.collection.mutable.ListBuffer
import scala.concurrent._
import scala.concurrent.duration._
import scala.util.control.Breaks._

/**
  * Created by culim on 2/24/16.
  */
object UCT  {

    implicit val timeout = Timeout(10 seconds)

    println("Create system")
    val system = ActorSystem("MCTS")
    var node : Node = null;

    def search(rootState : GameState, maxIterations : Int, verbose : Boolean = false) : Int = {

        val rootNode = system.actorOf(Node.props(state = rootState))
        val f_self = rootNode ? new Self()
        var root = Await.result(f_self,timeout.duration).asInstanceOf[Node]


        var state : GameState = null;
        for (iteration <- 1 to maxIterations) {
            //println("Iteration: "+iteration)
            node = root
            state = rootState.getCopy

            // Select
            //println("Select")
            val f_untriedActions = node.context.self ? new UntriedActions()
            var untriedActions = Await.result(f_untriedActions,timeout.duration).asInstanceOf[Set[Int]]
            val f_childrens = node.context.self ? new Children()
            var childrens = Await.result(f_childrens,timeout.duration).asInstanceOf[ListBuffer[Node]]

            breakable {
                while (untriedActions.isEmpty && childrens.nonEmpty) {
                    // Node has exhausted all actions -- fully expanded
                    // Node is non-terminal -- still has children to explore
                    val f_node = node.context.self ? new SelectChild()
                    var selected_node = Await.result(f_node, timeout.duration).asInstanceOf[Node]
                    //println("selected..." + node)
                    if (node.untriedActions.isEmpty) {
                        break
                    }
                    val f_action = selected_node.context.self ? new Action()
                    var action = Await.result(f_action, timeout.duration).asInstanceOf[Int]
                    //println("action..." + action)
                    state.doAction(action)
                }
            }

            // Expand
            //println("Expand")
            val f2_untriedActions = node.context.self ? new UntriedActions()
            var untriedActions2 = Await.result(f2_untriedActions,timeout.duration).asInstanceOf[Set[Int]]
            if (untriedActions2 nonEmpty) {
                val action : Int = untriedActions2.toList(Random.nextInt(untriedActions2.size))
                state.doAction(action)
                node.context.self ! new AddChild(action, state)
            }

            // Rollout
            //println("Rollout")
            while (!state.getAvailableActions.isEmpty) {
                state.doAction(state.getAvailableActions.toList(Random.nextInt(state.getAvailableActions.size)))
            }

            // Backpropagate

            //println("Backpropagate")
            while (node != null) {
                val future_index = node.context.self ? new PlayerIndex()
                var index = Await.result(future_index,timeout.duration).asInstanceOf[Int]
                node.context.self ! new Update(state.getResult(index))
                //val future_parent = node.context.self ? new Parent()
                //var parent = Await.result(future_parent,timeout.duration).asInstanceOf[Node]
                node = node.parent_p
            }

        }


        if (verbose) {
            println(rootNode ? new TreeToString())
        }
        else {
            println(rootNode ? new ChildrenToString());
        }


        var last =  root.children.sortBy(_.numberOfVisits).last
        val f_action = last.context.self ? new Action()
        var action = Await.result(f_action,timeout.duration).asInstanceOf[Int]

        return action
    }

}
