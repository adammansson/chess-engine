package chess

// times moved istället för hasmoved

trait PieceType:
  val notation: String
  val color: PieceColor

  def pseudoLegalMoves(state: Vector[PieceType], position: Int): Set[Int]
  override def toString: String = 
    color match
      case White => notation
      case _ => notation.toLowerCase

enum PieceColor:
  case White, Black, NoColor

export PieceColor.*

def otherColor(color: PieceColor): PieceColor =
  color match
    case White => Black
    case _ => White

case class Pawn(color: PieceColor) extends PieceType:  
  val notation = "P"

  val sideMultiplier: Int =
    color match
      case White => 1 
      case _ => -1

  val baseRow: Set[Int] = 
    color match
      case White => (31 to 38).toSet
      case _ => (81 to 88).toSet
  
  def pseudoLegalMoves(state: Vector[PieceType], position: Int): Set[Int] = 
    val pseudoLegal = scala.collection.mutable.Set.empty[Int]
    
    if state(position + 10*sideMultiplier) == Empty() then
      if baseRow(position) then
        if state(position + 20*sideMultiplier) == Empty() then
        pseudoLegal += position + 20*sideMultiplier
      
      pseudoLegal += position + 10*sideMultiplier

    if state(position + 9*sideMultiplier).color == otherColor(color) then
      pseudoLegal += position + 9*sideMultiplier

    if state(position + 11*sideMultiplier).color == otherColor(color) then
      pseudoLegal += position + 11*sideMultiplier

    pseudoLegal.toSet

case class King(color: PieceColor) extends PieceType:
  val notation = "K"

  def pseudoLegalMoves(state: Vector[PieceType], position: Int): Set[Int] = 
    val pseudoLegal = Vector(-11, -10, -9, -1, 1, 9, 10, 11).map(i => position + i)
    pseudoLegal.filter(i => state(i).color != color && state(i) != OutOfBounds()).toSet

case class Queen(color: PieceColor) extends PieceType:
  val notation = "Q"

  def pseudoLegalMoves(state: Vector[PieceType], position: Int): Set[Int] = 
    val pseudoLegal = scala.collection.mutable.Set.empty[Int]
    
    val rays = Vector(-11, -10, -9, -1, 1, 9, 10, 11)
    var isBlocked = false

    for ray <- rays do
      var i = 1
      var next = position + ray*i
      while !isBlocked do
        if state(next) == Empty() then
          pseudoLegal += next
          
          i += 1
          next = position + ray*i

        else
          if state(next).color == otherColor(color) then
            pseudoLegal += next
          isBlocked = true
      
      isBlocked = false

    pseudoLegal.toSet

case class Rook(color: PieceColor) extends PieceType:
  val notation = "R"

  def pseudoLegalMoves(state: Vector[PieceType], position: Int): Set[Int] = 
    val pseudoLegal = scala.collection.mutable.Set.empty[Int]
    
    val rays = Vector(-10, -1, 1, 10)
    var isBlocked = false

    for ray <- rays do
      var i = 1
      var next = position + ray*i
      while !isBlocked do
        if state(next) == Empty()  then
          pseudoLegal += next
          
          i += 1
          next = position + ray*i

        else
          if state(next).color == otherColor(color) then
            pseudoLegal += next
          isBlocked = true
      
      isBlocked = false

    pseudoLegal.toSet

case class Knight(color: PieceColor) extends PieceType:
  val notation = "N"

  def pseudoLegalMoves(state: Vector[PieceType], position: Int): Set[Int] = 
    val pseudoLegal = Vector(-8, 8, -12, 12, -19, 19, -21, 21).map(i => position + i)
    pseudoLegal.filter(i => state(i).color != color && state(i) != OutOfBounds()).toSet

case class Bishop(color: PieceColor) extends PieceType:
  val notation = "B"

  def pseudoLegalMoves(state: Vector[PieceType], position: Int): Set[Int] = 
    val pseudoLegal = scala.collection.mutable.Set.empty[Int]

    val rays = Vector(-11, -9, 9, 11)
    var isBlocked = false

    for ray <- rays do
      var i = 1
      var next = position + ray*i
      while !isBlocked do
        if state(next) == Empty()  then
          pseudoLegal += next
          
          i += 1
          next = position + ray*i

        else
          if state(next).color == otherColor(color) then
            pseudoLegal += next
          isBlocked = true
      
      isBlocked = false

    pseudoLegal.toSet

case class Empty() extends PieceType:
  val notation = " "
  val color = NoColor
  
  def pseudoLegalMoves(state: Vector[PieceType], position: Int): Set[Int] = 
    Set()

case class OutOfBounds() extends PieceType:
  val notation = "?"
  val color = NoColor
  
  def pseudoLegalMoves(state: Vector[PieceType], position: Int): Set[Int] = 
    Set()