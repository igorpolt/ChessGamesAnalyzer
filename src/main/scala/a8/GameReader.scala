package a8


import akka.actor.{Actor, ActorLogging, PoisonPill}



final case class Piece2(piece: Char, color: Char, x: Int, y: Int)

/**
  * Created by igorpoltavskiy on 20/09/16.
  */
class GameReader  (meta: Map[String, String], moves: Seq[Move2])  extends Actor with ActorLogging {

  log.info("read game of " + meta("White") + " " + meta("Result") + " " + meta("Black") + " at " + meta("Event"))

  var board = Array.fill[Option[Piece2]](8, 8)(None)

  // preset all the pieces on the board
  board(0)(0) = Some(Piece2('R', 'w', 0, 0))
  board(1)(0) = Some(Piece2('N', 'w', 1, 0))
  board(2)(0) = Some(Piece2('B', 'w', 2, 0))
  board(3)(0) = Some(Piece2('Q', 'w', 3, 0))
  board(4)(0) = Some(Piece2('K', 'w', 4, 0))
  board(5)(0) = Some(Piece2('B', 'w', 5, 0))
  board(6)(0) = Some(Piece2('N', 'w', 6, 0))
  board(7)(0) = Some(Piece2('R', 'w', 7, 0))
  board(0)(1) = Some(Piece2('p', 'w', 0, 1))
  board(1)(1) = Some(Piece2('p', 'w', 1, 1))
  board(2)(1) = Some(Piece2('p', 'w', 2, 1))
  board(3)(1) = Some(Piece2('p', 'w', 3, 1))
  board(4)(1) = Some(Piece2('p', 'w', 4, 1))
  board(5)(1) = Some(Piece2('p', 'w', 5, 1))
  board(6)(1) = Some(Piece2('p', 'w', 6, 1))
  board(7)(1) = Some(Piece2('p', 'w', 7, 1))

  board(0)(7) = Some(Piece2('R', 'b', 0, 7))
  board(1)(7) = Some(Piece2('N', 'b', 1, 7))
  board(2)(7) = Some(Piece2('B', 'b', 2, 7))
  board(3)(7) = Some(Piece2('Q', 'b', 3, 7))
  board(4)(7) = Some(Piece2('K', 'b', 4, 7))
  board(5)(7) = Some(Piece2('B', 'b', 5, 7))
  board(6)(7) = Some(Piece2('N', 'b', 6, 7))
  board(7)(7) = Some(Piece2('R', 'b', 7, 7))
  board(0)(6) = Some(Piece2('p', 'b', 0, 6))
  board(1)(6) = Some(Piece2('p', 'b', 1, 6))
  board(2)(6) = Some(Piece2('p', 'b', 2, 6))
  board(3)(6) = Some(Piece2('p', 'b', 3, 6))
  board(4)(6) = Some(Piece2('p', 'b', 4, 6))
  board(5)(6) = Some(Piece2('p', 'b', 5, 6))
  board(6)(6) = Some(Piece2('p', 'b', 6, 6))
  board(7)(6) = Some(Piece2('p', 'b', 7, 6))

  def count() = board.foldLeft(0)( _ + _.filter(x => x.isDefined).size)

  def isFreeVertical(x: Int, y1: Int, y2: Int): Boolean = {
    if (y2 > y1)
      (for (i <- (y1 + 1) until y2) yield board(x)(i)).forall(_.isEmpty)
    else
      (for (i <- (y2 + 1) until y1) yield board(x)(i)).forall(_.isEmpty)
  }

  def isFreeHorizontal(y: Int, x1: Int, x2: Int): Boolean = {
    if (x2 > x1)
      (for (i <- (x1 + 1) until x2) yield board(i)(y)).forall(_.isEmpty)
    else
      (for (i <- (x2 + 1) until x1) yield board(i)(y)).forall(_.isEmpty)
  }


  def inPossibleMoves(piece: Piece2, m: Move2): Boolean = {
    piece.piece match {
      case 'p' =>
        if (piece.color == 'w')
          (piece.x == m.x2 && ((m.y2 - piece.y) == 1 || (piece.y == 1 && m.y2 == 3 && board(piece.x)(2).isEmpty ))) || (((piece.x-m.x2).abs == 1) && ((m.y2 - piece.y) == 1) && m.capture)
        else
          (piece.x == m.x2 && ((piece.y - m.y2) == 1 || (piece.y == 6 && m.y2 == 4 && board(piece.x)(5).isEmpty))) || (((piece.x-m.x2).abs == 1) && ((piece.y - m.y2) == 1) && m.capture)

      case 'R' =>
        (piece.x == m.x2 && ((piece.y - m.y2).abs == 1 || isFreeVertical(m.x2, piece.y, m.y2))) || (piece.y == m.y2 && ((piece.x - m.x2).abs == 1 || isFreeHorizontal(m.y2, piece.x, m.x2)))

      case 'B' =>
        (piece.y - m.y2).abs == (piece.x - m.x2).abs

      case 'N' =>
        ((piece.y - m.y2).abs == 1 && (piece.x - m.x2).abs == 2) || ((piece.y - m.y2).abs == 2 && (piece.x - m.x2).abs == 1)

      case _ =>
        true
    }
  }



  def addX1Y1(m: Move2): Move2 = {
    var pieces = Array.empty[Option[Piece2]]
    if (m.x1 != -1) {
      pieces = board(m.x1).filter(p => p.isDefined && p.get.color == m.color && p.get.piece == m.piece && inPossibleMoves(p.get, m))
      if (pieces.length != 1) println("Found pieces \u001B[31m" + pieces.length + "\u001B[37m of \u001B[32m" + m.color + m.piece + "\u001B[37m on " +  m.x1 + " (" + m.original + " in " + meta("White") + " vs " + meta("Black") + ")")
    } else if (m.y1 != -1) {
      pieces = board.flatten.filter {
        p =>
          p.isDefined && p.get.color == m.color && p.get.piece == m.piece && p.get.y == m.y1 && inPossibleMoves(p.get, m)
      }
      if (pieces.length != 1) println("Found pieces \u001B[31m" + pieces.length + "\u001B[37m of \u001B[32m" + m.color + m.piece + "\u001B[37m on " +  m.x1 + " (" + m.original + " in " + meta("White") + " vs " + meta("Black") + ")")
    } else {
      pieces = board.flatten.filter {
        p =>
          p.isDefined && p.get.color == m.color && p.get.piece == m.piece && inPossibleMoves(p.get, m)
      }
      if (pieces.length != 1) println("Found pieces \u001B[31m" + pieces.length + "\u001B[37m of \u001B[32m" + m.color + m.piece + "\u001B[37m to go to " +  m.x2 + ", " + m.y2 + " (" + m.original + " in " + meta("White") + " vs " + meta("Black") + ")")
    }
    if (pieces.length == 1)
      Move2(m.color, m.piece, m.capture, m.check, m.x2, m.y2, pieces(0).get.x, pieces(0).get.y, m.original)
    else
      Move2(m.color, m.piece, m.capture, m.check, m.x2, m.y2, m.x1, -1, m.original)

  }


  var GREEN_LIGHT: Boolean = true

  moves.foreach {
    m =>
      if (GREEN_LIGHT) {
//        println(printBoard())
        var move = m
        if (move.y1 == -1 || move.x1 == -1)
          move = addX1Y1(move)

        if (move.x1 != -1 && move.y1 != -1) {
          val p = board(move.x1)(move.y1)
          if (p.isDefined) {
            board(move.x2)(move.y2) = Some(Piece2(p.get.piece, move.color, move.x2, move.y2))
            board(move.x1)(move.y1) = None
          } else {
            println("ERROR cant find a piece " + move.color + " " + move.piece + " " + move.x1 + ", " + move.y1)
            printBoard()
            context.self ! PoisonPill
          }
        } else {

          if (move.color == 'w')
            println("\u001B[33m" + move.piece + " (" + move.x1 + ", " + move.y1 + ") to (" + move.x2 + ", " + move.y2 + ")\u001B[37m")
          else
            println("\u001B[36m" + move.piece + " (" + move.x1 + ", " + move.y1 + ") to (" + move.x2 + ", " + move.y2 + ")\u001B[37m")
          println("Cant do " + move.original + "\n" + printBoard())
          GREEN_LIGHT = false


          //        log.error("Cant find out x, y for " + move.color + " " + move.piece + " to " + move.x2 + ", " + move.y2 +
          //        " " + meta("White") + " vs " + meta("Black") + " at " + meta("Event"))
        }




      }
  }

  def printBoard(): String = {
    var sb = new StringBuilder()
    for(y <- Array(7, 6, 5, 4, 3, 2, 1, 0)) {
      for(x <- 0 to 7) {
        var field = board(x)(y)
        if (field.isEmpty)
          sb.append(". ")
        else
        if (field.get.color == 'w')
          sb.append("\u001B[33m" + field.get.piece + "\u001B[37m ")
        else
          sb.append("\u001B[36m" + field.get.piece + "\u001B[37m ")
      }
      sb.append("\n")
    }
    sb.append("\n--------------\n")
    sb.toString
  }

  if (GREEN_LIGHT) {
    log.info("game of " + meta("White") + " vs " + meta("Black") + " at " + meta("Event") + " is over, pieces on the board " + count())
  } else {
    log.info("game of " + meta("White") + " vs " + meta("Black") + " at " + meta("Event") + " has been stopped, pieces on the board " + count())
  }


  def receive = {
    case _ =>
  }

}
