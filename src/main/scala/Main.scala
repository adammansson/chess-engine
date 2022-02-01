package chess

object Main:
  def main(args: Array[String]): Unit =    
    // "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R"
    // "r3kq1r/8/8/8/8/8/8/R2BKQ1R"
    val localBoard = Board.fromFEN()
    // localBoard.divide(args(0).toInt, White)
    println(localBoard.perft(args(0).toInt, White))

    /* var turn = White

    println(localBoard)
    while true do
      println(localBoard.allPseudoLegalMoves(turn))
      val input = scala.io.StdIn.readLine("Your move: ")
      localBoard.makeLegalMove(Move.fromString(localBoard.state, input), turn)
      println(localBoard)
      turn = otherColor(turn) */

    