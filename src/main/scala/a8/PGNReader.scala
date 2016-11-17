package a8

import akka.actor.{Props, _}

import scala.collection.mutable.Buffer

final case class Move2(color: Char, piece: Char, capture: Boolean, check: Boolean, x2: Int, y2: Int, x1: Int = -1, y1: Int = -1, original: String)
final case class GameData(meta: Map[String, String], moves: Seq[Move2])

class PGNReader(var metaString: String, var movesString: String) extends Actor with ActorLogging {

  implicit val system = context.system

  val metaPattern = "(Event|Date|White|Black|Result|Opening|Variation) \"([A-Za-z0-9 ,-]+)\"".r
  val gameTurnPattern =
    "([0-9]+)\\. ([K,Q,R,B,N]?)([a-h1-8]?)([x]?)([a-h]{1}[1-8]{1}|O-O|O-O-O)([\\+]?)[ \n]([K,Q,R,B,N]?)([a-h1-8]?)([x]?)([a-h]{1}[1-8]{1}|O-O|O-O-O|1-0|1/2-1/2)([\\+]?)".r

  var meta = Map.empty[String, String]
  var moves = Buffer[Move2]()

  /*

  1. c4 e5 2. d3 Nc6 3. Nf3 f5 4. g3 Nf6 5. Bg2 Bb4+ 6. Bd2 Bxd2+ 7. Qxd2 O-O 8.
  Nc3 d6 9. O-O Bd7 10. Nd5 Nxd5 11. cxd5 Ne7 12. Qb4 Nxd5 13. Qxb7 c6 14. Nd2 Nb6
  15. Qa6 d5 16. Rac1 f4 17. Nf3 Qf6 18. Qa5 Kh8 19. b3 Bg4 20. Qc3 e4 21. Qxf6
    Rxf6 22. Nd4 f3 23. exf3 exf3 24. Bh1 Rc8 25. Rfe1 h6 26. b4 Na4 27. Re3 Rcf8
  28. h3 Bxh3 29. Rxf3 Bd7 30. Bg2 g5 31. Rxf6 Rxf6 32. Nf3 Kg7 33. Ne5 Be8 34.
  Bh3 h5 35. d4 Nb6 36. Rc3 Nc4 37. Nxc4 dxc4 38. Rxc4 Rd6 39. a3 Bf7 40. Rc5 Rxd4
    41. Rxg5+ Kf6 42. Rf5+ Kg6 43. Rc5 Rd1+ 44. Kh2 Bd5 45. Bg2 Rd2 46. Bxd5 cxd5
  47. Kg2 Kf5 48. Ra5 Ke4 49. Rxa7 d4 50. b5 Rb2 51. a4 Kd3 52. Rb7 Ra2 53. b6
    Rxa4 54. Rb8 1-0


       1 2 3 4  5 6(+)  7 8 9 10 11(+)
  ----------------------------
  List(1, , , , c4, , , , e5)
  List(2, , , , d3, N, , , c6)
  List(3, N, , , f3, , , , f5)
  List(4, , , , g3, N, , , f6)
  List(5, B, , , g2, B, , , b4+)
  List(6, B, , , d2, B, , x, d2+)
..
  */


  def getY(move: String): Int =
    try {
      if (move.length > 1)
        Integer.parseInt(move(1).toString) - 1
      else if (move.length == 1)
        Integer.parseInt(move(0).toString) - 1
      else
        -1
    } catch {
      case e: NumberFormatException =>
        -1
    }

  def getX(move: String): Int =
    if (move.length >= 1) {
      val x = move(0) - 97
      if (x >= 0 && x <=7) x else -1
    } else -1

  metaPattern.findAllIn(metaString).matchData.foreach {
    m => meta += m.group(1) -> m.group(2)
  }

  gameTurnPattern.findAllIn(movesString).matchData.foreach {
    m =>
      if (m.group(5) == "O-O") {
        moves.append(Move2('w', 'K', false, false, 6, 0, 4, 0, m.group(0)))
        moves.append(Move2('w', 'R', false, m.group(6) == "+", 5, 0, 7, 0, m.group(0)))
      } else if (m.group(5) == "O-O-O") {
        moves.append(Move2('w', 'K', false, false, 2, 0, 4, 0, m.group(0)))
        moves.append(Move2('w', 'R', false, m.group(6) == "+", 3, 0, 0, 0, m.group(0)))
      } else {
        moves.append(
          Move2(
            'w',
            m.group(2).lift(0).getOrElse('p'),
            m.group(4) == "x",
            m.group(6) == "+",
            getX(m.group(5)),
            getY(m.group(5)),
            getX(m.group(3)),
            getY(m.group(3)),
            m.group(0)
          )
        )
      }

      if (m.group(10) == "O-O") {
        moves.append(Move2('b', 'K', false, false, 6, 7, 4, 7, m.group(0)))
        moves.append(Move2('b', 'R', false, m.group(11) == "+", 5, 7, 7, 7, m.group(0)))
      } else if (m.group(10) == "O-O-O") {
        moves.append(Move2('b', 'K', false, false, 2, 7, 4, 7, m.group(0)))
        moves.append(Move2('b', 'R', false, m.group(11) == "+", 3, 7, 0, 7, m.group(0)))
      } else if (m.group(10) == "1-0") {
      } else if (m.group(10) == "1/2-1/2") {
      } else {
        moves.append(
          Move2(
            'b',
            m.group(7).lift(0).getOrElse('p'),
            m.group(9) == "x",
            m.group(6) == "+",
            getX(m.group(10)),
            getY(m.group(10)),
            getX(m.group(8)),
            getY(m.group(8)),
            m.group(0)
          )
        )
      }
  }

  context.self ! GameData(meta.toMap, moves.toSeq)

  def receive = {


    // This is the best place to filter the games by metadata
    // EXAMPLE: d.meta.lift("Result").isDefined && (d.meta("Result") == "0-1" || d.meta("Result") == "1-0")
    case d: GameData =>

      if (true)
        context.actorOf(Props(new GameReader(d.meta, d.moves)))


    case x: String =>
      printf(x)

  }


}

