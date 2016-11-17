package a8

import polarsol.Monitoring
import akka.actor.{Actor, ActorSystem, Props}

object Main {
  def main(args: Array[String]) {
  	
	val system = ActorSystem("chess")

	system.actorOf(Props(new PGNFileReader(args(0))))

  }
}