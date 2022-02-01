package chess
import scala.collection.mutable.ArrayBuffer

case class Board(initialState: Vector[PieceType]):
  import Board.*

  var state = initialState
  
  def kingPosition(moveColor: PieceColor): Int = 
    state.indexOf(King(moveColor))
      
  def allAttackedSquares(moveColor: PieceColor): Set[Int] = 
    val attackedSquares = scala.collection.mutable.Set.empty[Int]
    for position <- state.indices do
      if state(position).color == moveColor then
        attackedSquares ++= state(position).pseudoLegalMoves(state, position)
    
    attackedSquares.toSet

  def isInCheck(moveColor: PieceColor): Boolean = 
    allAttackedSquares(otherColor(moveColor))(kingPosition(moveColor))

  def allPseudoLegalMoves(moveColor: PieceColor): Vector[Move] =
    val allMoves = scala.collection.mutable.Set.empty[Move]
    for position <- state.indices do
      if state(position).color == moveColor then
        for next <- state(position).pseudoLegalMoves(state, position) do
          allMoves += Move(position, next, state(position), state(next))

    allMoves ++= availableCastlingMoves(moveColor)
    allMoves.toVector
  
  /* def isCheckMate(moveColor: PieceColor): Option[PieceColor] =
    if isInCheck(moveColor) || allLegalMoves(moveColor).size == 0 then 
      Some(moveColor)
    else if isInCheck(otherColor(moveColor)) || allLegalMoves(otherColor(moveColor)).size == 0 then 
      Some(otherColor(moveColor))
    else None */

  def availableCastlingMoves(moveColor: PieceColor): Set[Move] = 
    val legalCastlingMoves = scala.collection.mutable.Set.empty[Move]
    val possibleCastlingMoves = moveColor match
      case White => Vector(Move(25, 21, state(25), state(21)), Move(25, 28, state(25), state(28)))
      case _ => Vector(Move(95, 91, state(95), state(91)), Move(95, 98, state(95), state(98)))

    if !isInCheck(moveColor) then
      for move <- possibleCastlingMoves do
        if state(move.from) == King(moveColor) && state(move.to) == Rook(moveColor) then
          if (for i <- math.min(move.from, move.to) + 1 to  math.max(move.from, move.to) - 1 yield
            state(i) == Empty() && !allAttackedSquares(otherColor(moveColor))(i)
          ).forall(_ == true) then
            legalCastlingMoves += move
    
    legalCastlingMoves.toSet

  def stateAfterMove(move: Move, moveColor: PieceColor): Vector[PieceType] = 
    val tempState = state.toArray

    if move.toPiece == Empty() then
      tempState(move.to) = move.fromPiece
      tempState(move.from) = Empty()
  
    else if move.toPiece.color == otherColor(move.fromPiece.color) then
      tempState(move.to) = move.fromPiece
      tempState(move.from) = Empty()

    else
      val (newKingIndex, newRookIndex) = 
        move match
          case Move(25, 21, _, _) => (23, 24)
          case Move(25, 28, _, _) => (27, 26)
          case Move(95, 91, _, _) => (93, 94)
          case Move(95, 98, _, _) => (97, 96)

      tempState(newKingIndex) = move.fromPiece
      tempState(newRookIndex) = move.toPiece
      tempState(move.from) = Empty()
      tempState(move.to) = Empty()

    tempState.toVector

  def stateBeforeMove(move: Move, moveColor: PieceColor): Vector[PieceType] = 
    val tempState = state.toArray

    if tempState(move.to) == move.fromPiece then
      tempState(move.from) = move.fromPiece
      tempState(move.to) = move.toPiece
    
    else if move.fromPiece.color == otherColor(move.toPiece.color) then
      tempState(move.from) = move.fromPiece
      tempState(move.to) = move.toPiece

    else
      val (newKingIndex, newRookIndex) = 
        move match
          case Move(25, 21, _, _) => (23, 24)
          case Move(25, 28, _, _) => (27, 26)
          case Move(95, 91, _, _) => (93, 94)
          case Move(95, 98, _, _) => (97, 96)

      tempState(move.from) = tempState(newKingIndex)
      tempState(move.to) = tempState(newRookIndex)
      tempState(newKingIndex) = Empty()
      tempState(newRookIndex) = Empty()
    
    tempState.toVector

  def makeMove(move: Move, moveColor: PieceColor): Unit = 
    state = stateAfterMove(move, moveColor)

  def unmakeMove(move: Move, moveColor: PieceColor): Unit =
    state = stateBeforeMove(move, moveColor)

  def perft(depth: Int, moveColor: PieceColor): Double =
    var nodes = 0D
    val move_list = allPseudoLegalMoves(moveColor)
    
    //if depth == 0 then
    // 1D
    if depth == 1 then
      // move_list.length.toDouble
      var j = 0
      for move <- move_list do
         if !Board(stateAfterMove(move, moveColor)).isInCheck(moveColor) then 
          j += 1
      j
    
    else
      for move <- move_list do
        // val t = System.nanoTime()

        makeMove(move, moveColor)

        // println(System.nanoTime() - t)

        if !isInCheck(moveColor) then 
          nodes += perft(depth - 1, otherColor(moveColor))
        
        unmakeMove(move, moveColor)
        
      nodes

  def divide(depth: Int, moveColor: PieceColor): Unit =
    for move <- allPseudoLegalMoves(moveColor) do
      makeMove(move, moveColor)
      println(s"$move ${perft(depth - 1, otherColor(moveColor))}")
      unmakeMove(move, moveColor)

  override def toString = 
    val filteredState = state.filterNot(_ == OutOfBounds())
    val seperator = "   +---+---+---+---+---+---+---+---+"
    val files = s"     ${Vector("A", "B", "C", "D", "E", "F", "G", "H").mkString("   ")}"
    
    def rank(nbr: Int): String = 
      val r = (for index <- nbr*8 until nbr*8+8 yield s"| ${filteredState(index)} ").mkString
      s"${nbr+1}  $r|"
    
    s"${(for rankNbr <- 7 to 0 by -1 yield s"$seperator \n${rank(rankNbr)}\n").mkString}$seperator\n$files\n"
        
object Board:
  val mailbox120 = Vector(
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1,  0,  1,  2,  3,  4,  5,  6,  7, -1,
    -1,  8,  9, 10, 11, 12, 13, 14, 15, -1, 
    -1, 16, 17, 18, 19, 20, 21, 22, 23, -1,
    -1, 24, 25, 26, 27, 28, 29, 30, 31, -1, 
    -1, 32, 33, 34, 35, 36, 37, 38, 39, -1, 
    -1, 40, 41, 42, 43, 44, 45, 46, 47, -1, 
    -1, 48, 49, 50, 51, 52, 53, 54, 55, -1, 
    -1, 56, 57, 58, 59, 60, 61, 62, 63, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  )

  val mailbox64 = Vector(
    21, 22, 23, 24, 25, 26, 27, 28,
    31, 32, 33, 34, 35, 36, 37, 38,
    41, 42, 43, 44, 45, 46, 47, 48,
    51, 52, 53, 54, 55, 56, 57, 58,
    61, 62, 63, 64, 65, 66, 67, 68,
    71, 72, 73, 74, 75, 76, 77, 78,
    81, 82, 83, 84, 85, 86, 87, 88,
    91, 92, 93, 94, 95, 96, 97, 98,
  )

  def fromFEN(fen: String = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"): Board = 
    val fenStr = fen.split("/").toVector.reverse.flatten

    val position = new Array[PieceType](120)
    for i <- mailbox120.indices do
      if mailbox120(i) == -1 then position(i) = OutOfBounds()

    var j = 0
    for i <- fenStr.indices do
      if fenStr(i).isLetter then
        fenStr(i).toUpper.toString match
          case "P" => position(mailbox64(j)) = new Pawn(if fenStr(i).isUpper then White else Black)
          case "K" => position(mailbox64(j)) = new King(if fenStr(i).isUpper then White else Black)
          case "Q" => position(mailbox64(j)) = new Queen(if fenStr(i).isUpper then White else Black)
          case "R" => position(mailbox64(j)) = new Rook(if fenStr(i).isUpper then White else Black)
          case "N" => position(mailbox64(j)) = new Knight(if fenStr(i).isUpper then White else Black)
          case "B" => position(mailbox64(j)) = new Bishop(if fenStr(i).isUpper then White else Black)

      else if fenStr(i).isDigit then
        for k <- 0 until (fenStr(i).toInt - 48) do
          position(mailbox64(j)) = Empty()
          j += 1
        j -= 1

      j += 1

    Board(position.toVector)
