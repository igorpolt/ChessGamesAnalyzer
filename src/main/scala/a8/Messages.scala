package a8

sealed trait Message


final case class Heartbeat() extends Message

final case class Game(white: String, black: String, result: String,
                      opening:String, variation: String,
                      event: String, date: String,
                      moves: Seq[Move]) extends Message

final case class Move(isWhite: Boolean, index: Int,
                     x1: Int, y1: Int, x2: Int, y2: Int) extends Message