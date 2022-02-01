package chess


/* case class Move(from: Int, to: Int):
  override def toString: String = 
    val fromRank = Board.mailbox64.indexOf(from) / 8
    val fromFile = Board.mailbox64.indexOf(from) % 8

    val toRank = Board.mailbox64.indexOf(to) / 8
    val toFile = Board.mailbox64.indexOf(to) % 8

    s"${(fromFile + 97).toChar}${fromRank + 1}:${(toFile + 97).toChar}${toRank + 1}" */


case class Move(from: Int, to: Int, fromPiece: PieceType, toPiece: PieceType):
  
  override def toString: String = 
    lazy val fromRank = Board.mailbox64.indexOf(from) / 8
    lazy val fromFile = Board.mailbox64.indexOf(from) % 8
    lazy val toRank = Board.mailbox64.indexOf(to) / 8
    lazy val toFile = Board.mailbox64.indexOf(to) % 8
 
    /* lazy val fromNotation: String = 
      fromPiece match
        case Pawn(_) => ""
        case _ => fromPiece.notation */

    // s"$fromNotation${(toFile + 97).toChar}${toRank + 1}" 
    // s"${fromPiece.notation}${(fromFile + 97).toChar}${fromRank + 1}:${toPiece.notation}${(toFile + 97).toChar}${toRank + 1}"
    s"${(fromFile + 97).toChar}${fromRank + 1}${(toFile + 97).toChar}${toRank + 1}"

object Move:
  def fromString(state: Vector[PieceType], input: String): Move =
    val pInput = input.split(":").map(x => x.split("").toVector)
    /* Least significant File */
    val from = Board.mailbox64( 8*(pInput(0)(1).toInt - 1) + (pInput(0)(0).head.toInt - 97) )
    val to = Board.mailbox64( 8*(pInput(1)(1).toInt - 1) + (pInput(1)(0).head.toInt - 97) )

    Move(from, to, state(from), state(to))
    