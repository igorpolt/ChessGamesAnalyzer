package a8

import akka.actor.{Props, _}

import scala.io.Source

class PGNFileReader(file: String) extends Actor with ActorLogging {

  log.info("Start read the file " + file)

  implicit val system = context.system

  var meta = ""
  var moves = ""
  var games = Map.empty[String, String]

  var i = 0
  Source.fromFile(file).getLines().foreach {
    line =>
      if (line.isEmpty) {
        if (meta.length > 3 && moves.length >= 1) {
          games += meta -> moves
          meta = ""
          moves = ""
        }
      } else if(line.charAt(0) == '[') {
        meta += line + ";"
      } else {
        moves += line + " "
      }
  }

  games.foreach {
    game =>
      context.actorOf(Props(new PGNReader(game._1, game._2)))

  }

  log.info("EOF total " + games.size)


  def receive = {

    case x: String =>
      printf(x)

  }


}

