import scala.io.Source
import java.io.{FileNotFoundException, IOException}
import com.whitehatgaming.UserInputFile

// Domain models
sealed trait Color
case object White extends Color
case object Black extends Color

sealed trait Piece {
  def color: Color
  def symbol: Char
}
case class King(color: Color) extends Piece { def symbol = if (color == White) 'K' else 'k' }
case class Queen(color: Color) extends Piece { def symbol = if (color == White) 'Q' else 'q' }
case class Rook(color: Color) extends Piece { def symbol = if (color == White) 'R' else 'r' }
case class Bishop(color: Color) extends Piece { def symbol = if (color == White) 'B' else 'b' }
case class Knight(color: Color) extends Piece { def symbol = if (color == White) 'N' else 'n' }
case class Pawn(color: Color) extends Piece { def symbol = if (color == White) 'P' else 'p' }

case class Position(x: Int, y: Int)
case class Move(from: Position, to: Position)

// Board representation
type Board = Vector[Vector[Option[Piece]]]

// Game state
case class GameState(board: Board, currentPlayer: Color)

object ChessGame {
  private val BoardSize = 8

  // Initialize the chess board
  def initializeBoard(): Board = {
    val backRankPiecesWhite = Vector(Rook(White), Knight(White), Bishop(White), Queen(White), King(White), Bishop(White), Knight(White), Rook(White))
    val backRankPiecesBlack = Vector(Rook(Black), Knight(Black), Bishop(Black), Queen(Black), King(Black), Bishop(Black), Knight(Black), Rook(Black))
    
    Vector.tabulate(BoardSize, BoardSize) {
      case (0, file) => Some(backRankPiecesWhite(file))
      case (1, _) => Some(Pawn(White))
      case (6, _) => Some(Pawn(Black))
      case (7, file) => Some(backRankPiecesBlack(file))
      case _ => None
    }
  }

  // Make a move, print updates and return the new game state
  def makeMove(gameState: GameState, move: Move): Either[String, GameState] = {
    for {
      piece <- gameState.board(move.from.y)(move.from.x).toRight("No piece at the starting position")
      _ <- Either.cond(piece.color == gameState.currentPlayer, (), "Not the current player's piece")
      _ <- Either.cond(isValidMove(gameState.board, move, piece), (), "Invalid move")
      newBoard = updateBoard(gameState.board, move)
      _ <- Either.cond(!isInCheck(newBoard, gameState.currentPlayer), (), "Move leaves the player in check")

    } yield {

      // Get the piece that's moving
      val movingPiece = gameState.board(move.from.y)(move.from.x)

      // Check if there's a piece at the target position (capture)
      val targetPiece = gameState.board(move.to.y)(move.to.x)

      // Print move information
      movingPiece.map(piece =>
        println(s"${piece.color} ${piece.getClass.getSimpleName} moved from " +
          s"${toChessNotation(move.from)} to ${toChessNotation(move.to)}")
      )
      
      // Print if there was a capture
      for {
        movingPiece <- movingPiece
        targetPiece <- targetPiece
      } yield {
        println(s"${movingPiece.color} ${movingPiece.getClass.getSimpleName} " +
          s"captured ${targetPiece.color} ${targetPiece.getClass.getSimpleName} at " +
          s"${toChessNotation(Position(move.to.x, move.to.y))}")
      }

      val newPlayer = if (gameState.currentPlayer == White) Black else White
      val newState = GameState(newBoard, newPlayer)
      
      if (isInCheck(newBoard, newPlayer)) println(s"$newPlayer is in check!")
      
      newState
    }
  }

  // Check if a move is valid for a given piece
  private def isValidMove(board: Board, move: Move, piece: Piece): Boolean = {
    !board(move.to.y)(move.to.y).exists(targetPiece => targetPiece.color == piece.color) &&
     {piece match {
      case King(_) => isValidKingMove(move)
      case Queen(_) => isValidQueenMove(board, move)
      case Rook(_) => isValidRookMove(board, move)
      case Bishop(_) => isValidBishopMove(board, move)
      case Knight(_) => isValidKnightMove(move)
      case Pawn(_) => isValidPawnMove(board, move, piece.color)
    }}
  }

  // Implement move validation for each piece type
  private def isValidKingMove(move: Move): Boolean =
    Math.abs(move.from.x - move.to.x) <= 1 && Math.abs(move.from.y - move.to.y) <= 1

  private def isValidQueenMove(board: Board, move: Move): Boolean =
    isValidRookMove(board, move) || isValidBishopMove(board, move)

  private def isValidRookMove(board: Board, move: Move): Boolean =
    (move.from.x == move.to.x || move.from.y == move.to.y) && !isPathBlocked(board, move)

  private def isValidBishopMove(board: Board, move: Move): Boolean =
    Math.abs(move.from.x - move.to.x) == Math.abs(move.from.y - move.to.y) && !isPathBlocked(board, move)

  private def isValidKnightMove(move: Move): Boolean =
    (Math.abs(move.from.x - move.to.x) == 2 && Math.abs(move.from.y - move.to.y) == 1) ||
    (Math.abs(move.from.x - move.to.x) == 1 && Math.abs(move.from.y - move.to.y) == 2)

  private def isValidPawnMove(board: Board, move: Move, color: Color): Boolean = {
    val direction = if (color == White) 1 else -1
    val startRank = if (color == White) 1 else 6

    def isForwardMove = move.from.x == move.to.x && move.to.y == move.from.y + direction && board(move.to.y)(move.to.x).isEmpty
    def isDoubleMove = move.from.x == move.to.x && move.to.y == move.from.y + 2 * direction && move.from.y == startRank &&
                       board(move.to.y)(move.to.x).isEmpty && board(move.from.y + direction)(move.from.x).isEmpty
    def isCapture = Math.abs(move.from.x - move.to.x) == 1 && move.to.y == move.from.y + direction &&
                    board(move.to.y)(move.to.x).exists(_.color != color)

    isForwardMove || isDoubleMove || isCapture
  }

  // Check if the path between two positions is blocked
  private def isPathBlocked(board: Board, move: Move): Boolean = {
    val xDirection = (move.to.x - move.from.x).sign
    val yDirection = (move.to.y - move.from.y).sign

    def pathPositions: Seq[Position] = {
      val steps = Math.max(Math.abs(move.to.x - move.from.x), Math.abs(move.to.y - move.from.y)) - 1
      (1 to steps).map(i => Position(move.from.x + i * xDirection, move.from.y + i * yDirection))
    }

    pathPositions.exists(pos => board(pos.y)(pos.x).isDefined)
  }

  private def updateBoard(board: Board, move: Move): Board = {
    // Get the piece that's moving
    val movingPiece = board(move.from.y)(move.from.x)

    // Check if there's a piece at the target position (capture)
    val targetPiece = board(move.to.y)(move.to.x)

    // Create a new board with the piece removed from its starting position
    val boardWithoutPiece = board.updated(move.from.y,
      board(move.from.y).updated(move.from.x, None)
    )

    // Place the piece at its new position
    val updatedBoard = boardWithoutPiece.updated(move.to.y,
      boardWithoutPiece(move.to.y).updated(move.to.x, movingPiece)
    )
    
    updatedBoard
  }

  // Check if a player is in check
  def isInCheck(board: Board, color: Color): Boolean = {
    val kingPosition = findKing(board, color)
    val opponentColor = if (color == White) Black else White

    board.zipWithIndex.exists { case (row, y) =>
      row.zipWithIndex.exists { case (cell, x) =>
        cell.exists { piece =>
          piece.color == opponentColor && isValidMove(board, Move(Position(x, y), kingPosition), piece)
        }
      }
    }
  }

  // Find the position of the king for a given color
  private def findKing(board: Board, color: Color): Position = {
    val kingPosition = for {
      (row, y) <- board.zipWithIndex
      (cell, x) <- row.zipWithIndex
      if cell.exists(p => p.isInstanceOf[King] && p.color == color)
    } yield Position(x, y)
    kingPosition.head
  }
  
  def toChessNotation(pos: Position): String = {
    val file = ('a' + pos.x).toChar // Convert x (0-7) to file (a-h)
    val rank = (pos.y + 1).toString // Convert y (0-7) to rank (1-8)
    s"$file$rank"
  }

  // Render the chess board as a string
  def renderBoard(board: Board): String = {
    val files = "      a     b     c     d     e     f     g     h  "
    val separator = "   +" + ("-----+" * BoardSize)

    // Renders the board with ranks (1-8) on the left, starting from the bottom
    val boardRows = board.zipWithIndex.reverse.map { case (row, y) =>
      val cells = row.map(_.map(_.symbol).getOrElse(' ')).mkString("|  ", "  |  ", "  |")
      f"${y + 1}%2d $cells${y + 1}%2d" // Convert index to rank (index + 1)
    }

    // Combines files row, separator, and board rows
    (Seq(files, separator) ++ boardRows.flatMap(row => Seq(row, separator)) :+ files).mkString("\n")
  }
}

object ChessApp {
  def main(args: Array[String]): Unit = {
    // Check if a moves file is provided as a command-line argument
    if (args.length != 1) {
      println("Usage: sbt \"run <moves_file_path>\"")
      System.exit(1)
    }

    val movesFile = args(0)
    val initialState = GameState(ChessGame.initializeBoard(), White)

    try {
      val inputFile = new UserInputFile(movesFile)
      processGame(initialState, inputFile)
    } catch {
      case e: FileNotFoundException =>
        println(s"Error: The specified file '$movesFile' was not found")
      case e: IOException =>
        println(s"Error reading moves: ${e.getMessage}")
      case e: Exception =>
        println(s"An unexpected error occurred: ${e.getMessage}")
    }
  }

  private def processGame(gameState: GameState, inputFile: UserInputFile): Unit = {
    Option(inputFile.nextMove()) match {
      case Some(moveArray) if moveArray.forall(n => n >= 0 && n < 8) =>
        val move = Move(
          Position(moveArray(0), 7 - moveArray(1)),
          Position(moveArray(2), 7 - moveArray(3))
        )
        println(s"\nProcessing ${gameState.currentPlayer} move from ${ChessGame.toChessNotation(move.from)} to ${ChessGame.toChessNotation(move.to)}")

        ChessGame.makeMove(gameState, move) match {
          case Right(newState) =>
            println(ChessGame.renderBoard(newState.board))
            println()
            processGame(newState, inputFile)
          case Left(errorMsg) =>
            println(s"Invalid move: $errorMsg")
            processGame(gameState, inputFile)
        }
      case Some(_) =>
        println("Error in moves file format")
      case None =>
        println("No more moves available - GAME OVER")
    }
  }
}
