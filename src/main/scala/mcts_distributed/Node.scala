package mcts_distributed

import akka.actor.{Actor, ActorRef, Props}
import game.GameState
import mcts_distributed.Node._
import mcts_distributed.UCT.{system, timeout}

import scala.collection.mutable.ListBuffer
import scala.concurrent.{Await, Future}
import akka.pattern.{ask, pipe}
import akka.util.Timeout

/**
  * Created by culim on 2/24/16.
  */

object Node {

  def props(action : Int = -1, parent : Node = null, state : GameState = null) = Props(classOf[Node], action,parent,state)

  case class NumberOfWins()
  case class NumberOfVisits()
  case class Children()
  case class UntriedActions()
  case class PlayerIndex()
  case class Epsilon()
  case class SelectChild()
  case class Update( result : Double)
  case class AddChild(action : Int, state : GameState )
  case class ToString()
  case class TreeToString()
  case class ChildrenToString()
  case class Action()
  case class Parent()
  case class Self()

}


class Node(action : Int = -1, parent : Node = null, state : GameState = null) extends Actor with akka.actor.ActorLogging{

    var action_p : Int = action
    var parent_p : Node = parent
    var numberOfWins : Double = 0
    var numberOfVisits : Int = 0
    var children : ListBuffer[Node] = ListBuffer.empty
    var untriedActions : Set[Int] = state.getAvailableActions
    var playerIndex : Int = state.getLastPlayerWhoMoved
    val epsilon : Double = 1e-6

    def selectChild : Node = {
        val sortedChildren = children.map( node => {

          val f_wins = node.context.self ? new NumberOfWins()
          var numberOfWins = Await.result(f_wins,timeout.duration).asInstanceOf[Double]
          val f_visits = node.context.self ? new NumberOfVisits()
          var numberOfVisits = Await.result(f_visits,timeout.duration).asInstanceOf[Int]
          var a  = 0.0
          if (numberOfWins > 0.0 || numberOfVisits > 0){
            a = (numberOfWins/numberOfVisits)
          }
          //println(a)
          var b = Math.sqrt(2 * Math.log(this.numberOfVisits+1))
          //println(b)
          var c = numberOfVisits + epsilon
          //println(c)
          var selection = a+ (b/c)
          (node,selection)
        }).sortBy(_._2)
        //println("To select:"+sortedChildren)
        return sortedChildren.last._1
    }

    def update(result : Double) : Unit = {
      //println("numberOfVisits previous:"+this.numberOfVisits)
      //println("numberOfWins previous:"+this.numberOfWins)
      this.numberOfVisits = this.numberOfVisits + 1;
      this.numberOfWins = this.numberOfWins + result;
      //println("numberOfVisits post:"+this.numberOfVisits)
      //println("numberOfWins post:"+this.numberOfWins)
    }

    def addChild(action : Int, state : GameState) : ActorRef = {

        val n = system.actorOf(Node.props(action,this,state))
        untriedActions -= action

        val f_self : Future[Any] = n ? new Self()
        var self = Await.result(f_self,timeout.duration).asInstanceOf[Node]

        children +=self

        return n
    }

    override def toString() : String = {
        return  s"[A: $action; " +
                s"W/V: ${numberOfWins}/${numberOfVisits} = ${numberOfWins.toDouble/numberOfVisits}; " +
                s"U: ${untriedActions}"
    }


    def treeToString(indent : Int  ) : String = {
        var s : String = indentString(indent) + this.toString();
        for (c <- children) {
            s += c.treeToString (indent + 1)
        }

        return s
    }


    def indentString(indent : Int) : String = {
        var s = "\n"
        for (i <- 1 to indent) {
            s += "| "
        }

        return s
    }


    def childrenToString() : String = {
        var s = ""
        for (c <- children)  {
            s += c.toString() + "\n"
        }
        return s
    }

    def receive : Receive = {

      case  x : NumberOfWins => sender ! this.numberOfWins
      case  x : NumberOfVisits => sender ! this.numberOfVisits
      case  x : Children => sender ! this.children.sortBy(_.numberOfVisits)
      case  x : UntriedActions => sender ! this.untriedActions
      case  x : PlayerIndex => sender ! this.playerIndex
      case  x : Epsilon => sender ! this.epsilon
      case  x : SelectChild => sender ! this.selectChild
      case  x : Update => sender ! this.update(x.result)
      case  x : AddChild => sender ! this.addChild(x.action,x.state)
      case  x : ToString => sender ! this.toString()
      case  x : TreeToString => sender ! this.treeToString(0)
      case  x : ChildrenToString => sender ! this.childrenToString()
      case  x : Action => sender ! this.action_p
      case  x : Parent => sender ! this.parent_p
      case  x : Self => sender ! this
      case  x : Any => sender ! x
      case  x : Node => sender ! x
      case  _ => sender ! _

    }

}
