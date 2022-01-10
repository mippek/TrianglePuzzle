package puzzle.tests

import puzzle._  
import org.scalatest._
import org.scalactic._
import org.junit.Test
import org.junit.Assert._
import scala.collection.mutable.Buffer


class PuzzleTest extends FlatSpec {
  
  "Piece" should "add coordinates correctly" in {
    val piece = new Piece('b', 'c', 'A', 1)
    assume(piece.getCoords === (-1, -1))
    
    piece.addCoords(2, 1)
    assert(piece.getCoords === (2, 1))
    
    piece.addCoords(4, 2)
    assert(piece.getCoords === (4, 2))
  }
  
  it should "remove coordinates correctly" in {
    val piece = new Piece('c', 'c', 'c', 1)
    piece.addCoords(1, 1)
    assume(piece.getCoords === (1, 1))
    
    piece.removeCoords()
    assert(piece.getCoords === (-1, -1))
    
    piece.removeCoords()
    assert(piece.getCoords === (-1, -1))
  }
  
  it should "change its position when rotated" in {
    val piece = new Piece('A', 'D', 'B', 1)
    assume(piece.position === 1)
    
    piece.rotate()
    assert(piece.position === 2)
    piece.rotate()
    assert(piece.position === 3)
    piece.rotate()
    assert(piece.position === 4)
    piece.rotate()
    assert(piece.position === 5)
    piece.rotate()
    assert(piece.position === 6)
    piece.rotate()
    assert(piece.position === 1)
  }
  
  it should "convert its sides to match the position it is in" in {
    val piece = new Piece('A', 'B', 'C', 1)
    val converted = piece.convertPos
    
    assert(converted._1 === 'A')
    assert(converted._2 === 'B')
    assert(converted._3 === 'O')
    assert(converted._4 === 'C')
  }
  
  it should "return true when samePiece is called and the sides of the two pieces match, otherwise return false" in {
    val piece1 = new Piece('a', 'b', 'd', 1)
    val piece2 = new Piece('a', 'b', 'd', 4)
    val piece3 = new Piece('a', 'c', 'd', 1)
    
    assert(piece1.samePiece(piece2) === true)
    assert(piece1.samePiece(piece3) === false)
  }
  
  "Board" should "be empty and have padding around it when created" in {
    val board = new Board
    val padPiece = new Piece('x', 'x', 'x', 1)
    
    assert(board.getBoard(0)(3).get.samePiece(padPiece))
    assert(board.getBoard(1)(1).get.samePiece(padPiece))
    assert(board.getBoard(1)(7).get.samePiece(padPiece))
    assert(board.getBoard(2)(0).get.samePiece(padPiece))
    assert(board.getBoard(2)(8).get.samePiece(padPiece))
    
    assert(board.getBoard(1)(2) === None)
    assert(board.getBoard(1)(2) === None)
    assert(board.getBoard(1)(6) === None)
    assert(board.getBoard(2)(1) === None)
    assert(board.getBoard(2)(7) === None)
    assert(board.getBoard(4)(4) === None)
  }
  
  it should "return pieces on given coords correctly" in {
    val board = new Board
    assume(board.pieceOnCoords(1, 3) === None)
    
    val piece = new Piece('d', 'c', 'b', 1)
    board.addPiece(piece, 1, 3)
    val pieceOnCoords1 = board.pieceOnCoords(1, 3)
    assert(pieceOnCoords1 === Some(piece))
    val pieceOnCoords2 = board.pieceOnCoords(1, 7)
    assert(pieceOnCoords2 === None)
    val pieceOnCoords3 = board.pieceOnCoords(2, 1)
    assert(pieceOnCoords3 === None)
  }
  
  it should "add pieces in correct places" in {
    val board = new Board
    
    val piece1 = new Piece('A', 'b', 'D', 1)
    assume(board.getBoard(1)(2) === None)
    val success1 = board.addPiece(piece1, 1, 1)
    assert(success1 === true)
    assert(board.getBoard(1)(2) === Some(piece1))
    
    val piece2 = new Piece('d', 'b', 'C', 1)
    assume(board.getBoard(4)(6) === None)
    val success2 = board.addPiece(piece2, 5, 4)
    assert(success2 === true)
    assert(board.getBoard(4)(6) === Some(piece2))
    
    val piece3 = new Piece('C', 'a', 'C', 1)
    assume(board.getBoard(2)(3) === None)
    val success3 = board.addPiece(piece3, 3, 2)
    assert(success3 === true)
    assert(board.getBoard(2)(3) === Some(piece3)) 
  }
  
  it should "not add pieces outside the board" in {
    val board = new Board
    val piece = new Piece('d', 'b', 'A', 1)
    val padPiece = new Piece('x', 'x', 'x', 1)
    
    assume(board.getBoard(1)(1).get.samePiece(padPiece))
    val success1 = board.addPiece(piece, 0, 1)
    assert(success1 === false)
    assert(board.getBoard(1)(1).get.samePiece(padPiece))
    
    assume(board.getBoard(0)(0).get.samePiece(padPiece))
    val success2 = board.addPiece(piece, 0, 0)
    assert(success2 === false)
    assert(board.getBoard(0)(0).get.samePiece(padPiece))
  }
  
  it should "not add a piece if there already is a piece" in {
    val board = new Board
    val piece1 = new Piece('A', 'a', 'a', 1)
    board.addPiece(piece1, 1, 1)
    assume(board.getBoard(1)(2) === Some(piece1))
    
    val piece2 = new Piece('b', 'C', 'D', 1)
    val success = board.addPiece(piece2, 1, 1)
    assert(success === false)
    assert(board.getBoard(1)(2) === Some(piece1))
  }
  
  it should "rotate a piece when added to the board if it is not in correct position" in {
    val board = new Board
    
    val piece1 = new Piece('d', 'b', 'b', 1)
    assume(piece1.position === 1)
    board.addPiece(piece1, 2, 1)
    assert(piece1.position === 2)
    
    val piece2 = new Piece('a', 'a', 'a', 6)
    assume(piece2.position === 6)
    board.addPiece(piece2, 1, 2)
    assert(piece2.position === 1)
    
    val piece3 = new Piece('C', 'A', 'A', 3)
    assume(piece3.position === 3)
    board.addPiece(piece3, 2, 4)
    assert(piece3.position === 3)
    
    val piece4 = new Piece('d', 'A', 'b', 4)
    assume(piece4.position === 4)
    board.addPiece(piece4, 1, 3)
    assert(piece4.position === 4)
  }
  
  it should "remove pieces that are on the board" in {
    val board = new Board
    val piece = new Piece('d', 'd', 'd', 1)
    board.addPiece(piece, 1, 1)
    assume(board.getBoard(1)(2) === Some(piece))
    
    val success = board.removePiece(piece)
    assert(success === true)
    assert(board.getBoard(1)(2) === None)
  }
  
  it should "not do anything if the removed piece is not on the board" in {
    val board = new Board
    val piece = new Piece('b', 'b', 'a', 1)
    
    val success = board.removePiece(piece)
    assert(success === false)
  }
  
  it should "add correct coordinates for added pieces" in {
    val board = new Board
    val piece = new Piece('d', 'D', 'D', 1)
    assume(piece.getCoords === (-1, -1))
    
    board.addPiece(piece, 1, 1)
    assert(piece.getCoords === (1, 1))
  }
  
  it should "remove the coordinates of the removed piece" in {
    val board = new Board
    val piece = new Piece('B', 'c', 'a', 1)
    board.addPiece(piece, 1, 1)
    assume(piece.getCoords === (1, 1))
    
    board.removePiece(piece)
    assert(piece.getCoords === (-1, -1))
  }
  
  "Pile" should "return the correct pieces on some given indices" in {
    val pile = new PileOfPieces
    assume(pile.isEmpty)
    
    val p1 = pile.pieceOnIndex(5)
    assert(p1 === None)
    
    val piece = new Piece('D', 'd', 'C', 1)
    pile.addPiece(piece)
    val p2 = pile.pieceOnIndex(0)
    assert(p2 === Some(piece))
    val p3 = pile.pieceOnIndex(3)
    assert(p3 === None)
  }
  
  it should "add every piece as its last piece" in {
    val pile = new PileOfPieces
    assume(pile.isEmpty)
    
    val piece1 = new Piece('B', 'B', 'c', 1)
    pile.addPiece(piece1)
    assert(pile.pieceOnIndex(pile.size - 1).get === piece1)
    
    val piece2 = new Piece('C', 'c', 'A', 1)
    pile.addPiece(piece2)
    assert(pile.pieceOnIndex(pile.size - 1).get === piece2)
  }
  
  it should "not add pieces that already are in the pile" in {
    val pile = new PileOfPieces
    val piece1 = new Piece('c', 'D', 'c', 1)
    pile.addPiece(piece1)
    val piece2 = new Piece('A', 'd', 'b', 1)
    pile.addPiece(piece2)
    assume(pile.pieceOnIndex(pile.size - 1).get === piece2)
    
    pile.addPiece(piece1)
    assert(pile.pieceOnIndex(pile.size - 1).get === piece2)
  }
  
  it should "take pieces from the pile" in {
    val pile = new PileOfPieces
    val piece = new Piece('D', 'c', 'b', 1)
    pile.addPiece(piece)
    assume(pile.contains(piece))
    
    pile.takePiece(piece)
    assert(pile.isEmpty)
  }
  
  it should "not try and take pieces that are not in the pile" in {
    val pile = new PileOfPieces
    val piece = new Piece('a', 'd', 'c', 1)
    pile.takePiece(piece)
    assert(pile.isEmpty)
  }
  
  it should "update the current piece correctly when pieces are added or taken" in {
    val pile = new PileOfPieces
    assume(pile.currentPiece === None)
    
    val piece1 = new Piece('c', 'c', 'c', 1)
    pile.addPiece(piece1)
    assert(pile.currentPiece === Some(piece1))
    
    val piece2 = new Piece('C', 'C', 'C', 1)
    pile.addPiece(piece2)
    assert(pile.currentPiece === Some(piece2))
    
    pile.takePiece(piece2)
    pile.takePiece(piece1)
    assert(pile.currentPiece === None)
  }
  
  it should "flip through the pile correctly" in {
    val pile = new PileOfPieces
    val piece1 = new Piece('B', 'A', 'A', 1)
    val piece2 = new Piece('c', 'B', 'a', 1)
    val piece3 = new Piece('a', 'd', 'b', 1)
    pile.addPiece(piece1)
    pile.addPiece(piece2)
    pile.addPiece(piece3)
    assume(pile.currentPiece === Some(piece3))
    
    pile.flipRight()
    assert(pile.currentPiece === Some(piece1))
    pile.flipRight()
    assert(pile.currentPiece === Some(piece2))
    pile.flipRight()
    assert(pile.currentPiece === Some(piece3))
    
    pile.flipLeft()
    assert(pile.currentPiece === Some(piece2))
    pile.flipLeft()
    assert(pile.currentPiece === Some(piece1))
    pile.flipLeft()
    assert(pile.currentPiece === Some(piece3))
    
    pile.takePiece(piece1)
    assert(pile.currentPiece === Some(piece3))
    
    pile.takePiece(piece3)
    assert(pile.currentPiece === Some(piece2))
    pile.flipLeft()
    pile.flipRight()
    assert(pile.currentPiece === Some(piece2))
    
    pile.takePiece(piece2)
    pile.flipLeft()
    pile.flipRight()
    assert(pile.currentPiece === None)
  }
  
  it should "empty the pile when empty() is called if it is not empty alredy" in {
    val pile = new PileOfPieces
    assume(pile.isEmpty)
    
    pile.empty()
    assert(pile.isEmpty)
    
    val piece1 = new Piece('c', 'a', 'B', 1)
    pile.addPiece(piece1)
    pile.empty()
    assert(pile.isEmpty)
    
    pile.addPiece(piece1)
    val piece2 = new Piece('B', 'b', 'B', 1)
    pile.addPiece(piece2)
    pile.empty()
    assert(pile.isEmpty)
  }
  
  "Game" should "create a solution and store it correctly when the game is started" in {
    val game = new Game
    assume(game.solution.forall( _.forall( _ == null ) ))
    
    game.startGame()
    val solution = game.solution
    assert(solution.forall( _.forall( p => p != null && p != None  ) ))
    val solFound = Buffer[Boolean]()
    for {
      i <- 0 until solution.length
      j <- 0 until solution(i).length
    } {
      val p = solution(i)(j).get
      if (!p.samePiece(game.board.padPiece)) {
        val l = solution(i)(j - 1).get.convertPos._2
        val u = solution(i - 1)(j).get.convertPos._4
        solFound += game.correctSides(p, l, u)
      }
    }
    assert(solFound.forall( _ == true ))
    
    assert(game.pile.size == 24)
    for (i <- 0 until 24) {
      val p = game.pile.pieceOnIndex(0).get
      game.pile.takePiece(p)
      assert(!game.pile.contains(p))
      game.pile.addPiece(p)
    }
  }
  
  it should "find the correct sides for pieces correctly" in {
    val game = new Game 
    val piece = new Piece('a', 'B', 'D', 1)
    
    assert(game.correctSides(piece, 'x', 'x') === true)
    assert(game.correctSides(piece, 'x', 'O') === true)
    assert(game.correctSides(piece, 'A', 'x') === true)
    assert(game.correctSides(piece, 'A', 'O') === true)
    assert(game.correctSides(piece, 'b', 'C') === false)
  }
  
  it should "find a correct solution" in {
    val game = new Game
    game.startGame()
    game.solveGame()
    
    val b = game.board
    val solFound = Buffer[Boolean]()
    for {
      i <- 0 until b.rows
      j <- 0 until b.columns
    } {
      val p = b.getBoard(i)(j).get
      if (!p.samePiece(game.board.padPiece)) {
        val l = b.getBoard(i)(j - 1).get.convertPos._2
        val u = b.getBoard(i - 1)(j).get.convertPos._4
        solFound += game.correctSides(p, l, u)
      }
    }
    assert(solFound.forall( _ == true ))
    
  }
  
}
