package puzzle

import scala.collection.mutable.Buffer 
import scala.util.Random

class Game {
  
  /* Creates the instances of a board and a pile. */
  val board = new Board
  val pile = new PileOfPieces
  
  /* Returns an array of the places on the board. */
  val piecePlaces = board.placesOnBoard
  
  /* Stores the generated solution for the game. If a solution is not yet generated, 
   * the array contains null-values. */
  private var sol = Array.ofDim[Option[Piece]](6, 9)
  
  /* A variable that determines whether the game has been started or not. */
  private var gameStarted = false
  
  /* Returns the solution-array for the game. */
  def solution = this.sol
  
  /* Fills the solution array with padding pieces.*/
  private def solInit() = {
    for {
      i <- 0 until this.sol.length
      j <- 0 until this.sol(i).length
    } {
      if (this.piecePlaces(i)(j) == 2 || (i == 5 && (j == 3 || j == 5))) {
        this.sol(i)(j) = Some(new Piece('x', 'x', 'x', 2))
      } else {
        this.sol(i)(j) = Some(new Piece('x', 'x', 'x', 1))
      }
    }
  }
  
  /* A helper method that takes a char as a parameter and returns the corresponding char. If the
   * given symbol is 'x', the method randomly chooses one symbol to return from a vector. */
  private def matchingSymbol(c: Char): Char = {
    val symbols = Vector('A', 'B', 'C', 'D', 'a', 'b', 'c', 'd')
    c match {
      case 'A' => 'a'
      case 'B' => 'b'
      case 'C' => 'c'
      case 'D' => 'd'
      case 'a' => 'A'
      case 'b' => 'B'
      case 'c' => 'C'
      case 'd' => 'D'
      case 'O' => 'O'  
      case 'x' => symbols(Random.nextInt(8))
    }
  }
  
  /* A helper method that generates a new piece. It takes four chars, the the sides surrounding the piece
   * on the board, as parameters and an integer that tells the position the piece is in. */
  private def generatePiece(pieceOnLeft: Piece, pieceOnRight: Piece, pieceAbove: Piece, pieceBeneath: Piece, n: Int): Piece = {
    if (n == 1) {
      val a = this.matchingSymbol(pieceOnLeft.convertPos._2)
      val b = this.matchingSymbol(pieceOnRight.convertPos._1)
      val c = this.matchingSymbol(pieceBeneath.convertPos._3)
      new Piece(a, b, c, 1)
    } else {
      val a = this.matchingSymbol(pieceOnLeft.convertPos._2)
      val b = this.matchingSymbol(pieceOnRight.convertPos._1)
      val c = this.matchingSymbol(pieceAbove.convertPos._4)
      new Piece(c, b, a, 2)
    }
  }
  
  /* Generates a new solution for the game. */
  private def generateSolution() = {
    
    /* Stores all of the generated pieces. */
    var existingPieces = Buffer[Piece]()
    
    this.solInit()
    
    /* Checks whether a piece already exists in the buffer of generated pieces. */
    def pieceExists(p: Piece) = {
      existingPieces.exists( _.samePiece(p) )
    }
    
    /* Generates pieces for each position in the solution. The method checks the generated piece
     * does not already exist. It adds the piece to the solution array, coords to the piece and the
     * piece to the buffer of existing pieces. */
    for {
      i <- 0 until this.sol.length
      j <- 0 until this.sol(i).length
    } {
      var piece = this.board.padPiece
      do {
        if (this.piecePlaces(i)(j) == 1) { 
          piece = this.generatePiece(this.sol(i)(j - 1).get, this.sol(i)(j + 1).get, this.sol(i - 1)(j).get, this.sol(i + 1)(j).get, 1)
        }
        if (this.piecePlaces(i)(j) == 2) {
          piece = this.generatePiece(this.sol(i)(j - 1).get, this.sol(i)(j + 1).get, this.sol(i - 1)(j).get, this.sol(i + 1)(j).get, 2)
        }
      } while (pieceExists(piece))
        
      if (this.piecePlaces(i)(j) != 0) { 
        this.sol(i)(j) = Some(piece)
        if (i == 1 || i == 4) {
          piece.addCoords(i, j - 1)
        } else {
          piece.addCoords(i, j)
        }
        /* Creates completely new instances of the pieces to avoid rotating the pieces in the solution. */
        existingPieces += new Piece(piece.left, piece.right, piece.bottom, piece.position)
      }  
      
    }
    
    /* Shuffles the pieces and rotates them a randomly. Adds the pieces to the pile. */
    for (i <- 1 to 24) {
      val shuffled = Random.shuffle(existingPieces)
      shuffled.foreach( _.rotate() )
      this.pile.addPiece(shuffled.head)
      existingPieces = shuffled.tail
    }
  } 
  
  /* Starts the game. The game can be started only if it has not already been started. */
  def startGame() = {
    if (!this.gameStarted) {
      this.generateSolution()
      this.gameStarted = true
    }
  }
  
  /* Continues a game that has previously been started. */
  def continueGame(situation: Array[Array[Option[Piece]]], pileSituation: Buffer[Piece], solution: Array[Array[Option[Piece]]]) = {
    if (!this.gameStarted) {
      /* Adds the pieces on the game board. */
      val boardArr = situation
      for {
        i <- 0 until boardArr.length
        j <- 0 until boardArr(i).length
      } {
        val p = boardArr(i)(j).getOrElse(this.board.padPiece)
        if (!p.samePiece(this.board.padPiece)) {
          if (i == 1 || i == 4) {
            this.board.addPiece(p, j - 1, i)
          } else {
            this.board.addPiece(p, j, i)
          }
        }
      }
      
      /* Adds the pieces to the pile. */
      val pileBuf = pileSituation
      for (p <- 0 until pileBuf.length) {
        this.pile.addPiece(pileBuf(p))
      }
      
      /* Updates the solution to match the solution of the continued game. */
      sol = solution 
      for {
        i <- 0 until this.sol.length
        j <- 0 until this.sol(i).length
      } {
        if (this.piecePlaces(i)(j) != 0) { 
          val p = this.sol(i)(j)
          if ((i == 1 || i == 4) && p != None) { p.get.addCoords(i, j - 1) } 
          else {
            if (p != None) {
              p.get.addCoords(i, j)
            }
          }  
        }
      }  
      
      /* If the continued situation is empty, e.g., there are no pieces or solution, the method
       * generates a new solution as the method startGame. */
      if (this.board.isEmpty && this.pile.isEmpty) this.generateSolution
      
      this.gameStarted = true
    }
  }
  
  /* Gives a hint for the player by removing the wrong pieces from the board and adding three or one piece
   * from the pile on the correct places board depending on how many pieces are left in the pile. The correct
   * places are the ones in the generated solution. */
  def getHelp() = {
    
    /* Adds a piece on its correct place on the board. If the place on the board already contains
     * a piece, removes the piece and adds this piece instead. Rotates the added pieces to the same
     * position as the pieces in the solution. */
    def addCorrectPiece(piece: Piece) = { 
          for {
            i <- 0 until this.sol.length
            j <- 0 until this.sol(i).length
          } {
            val solPiece = this.sol(i)(j).get
            if (solPiece.samePiece(piece)) {
              val coords = solPiece.getCoords
              val pieceOnBoard = this.board.pieceOnCoords(coords._2, coords._1)
              if (pieceOnBoard != None) {
                this.board.removePiece(pieceOnBoard.get)
                this.pile.addPiece(pieceOnBoard.get)
              }
              this.board.addPiece(piece, coords._2, coords._1)
              this.pile.takePiece(piece)
              while (piece.position != solPiece.position) { piece.rotate }
            }
          }  
    }
    
    /* Only gives hints if the game has been started. */
    if (this.gameStarted) {
      
      /* Removes the wrong pieces from the board. And rotates the correct pieces to the correct position. */
      for {
        i <- 0 until this.sol.length
        j <- 0 until this.sol(i).length
      } {
        val solPiece = this.sol(i)(j).get
        val boardPiece = this.board.getBoard(i)(j)
        if (boardPiece != None) {
          val piece = boardPiece.get
          if (piece.samePiece(solPiece)) { while (piece.position != solPiece.position) { piece.rotate } }
          else {
            this.board.removePiece(piece)
            this.pile.addPiece(piece)
          }
        }
      }
      
      /* Normally, adds three correct pieces on the board, if there are enough pieces left in the pile. */
      if (this.pile.size >= 3) {
        for (n <- 1 to 3) {
          val piece = this.pile.piecePile.head
          addCorrectPiece(piece)
        } 
      } else {
        
        /* If the size of the pile is less than three, only adds one piece on the board. */
        if (this.pile.size >= 1) {
          val piece = this.pile.piecePile.head
          addCorrectPiece(piece)
        }
      }
    }
  }
  
  /* Checks that a given piece has the correct symbols on its sides. It takes as parameters two
   * chars that represent the two symbols on the left and above a piece on the board. If both sides next to
   * a piece contains the symbol 'x', returns true. */
  def correctSides(p: Piece, l: Char, u: Char): Boolean = {
    val converted = p.convertPos
    (this.matchingSymbol(l) == converted._1 || l == 'x') && (this.matchingSymbol(u) == converted._3 || u == 'x')
  }
  
  /* Checks whether a solution is found on the board. */
  private def solutionFound: Boolean = {
    
    /* If the board is full checks that each piece on the board is correct. Otherwise returns false. */
    if (this.board.isFull) {
      val solFound = Buffer[Boolean]()
      for {
        i <- 0 until this.board.rows
        j <- 0 until this.board.columns
      } {
        val p = this.board.getBoard(i)(j).get
        if (!p.samePiece(this.board.padPiece)) {
          val l = this.board.getBoard(i)(j - 1).get.convertPos._2
          val u = this.board.getBoard(i - 1)(j).get.convertPos._4
          solFound += this.correctSides(p, l, u)
        }
      }
      solFound.forall( _ == true ) 
    } else { false }
  }
  
  /* Checks when the game is over, which is when the solution is found and the game has been started. */
  def gameOver = this.gameStarted && this.solutionFound
  
  /* Ends the game if the game is over. It empties the board and the pile of all pieces and empties 
   * the solution array. */
  def endGame() = {
    if (this.gameOver) {
      this.sol = Array.ofDim[Option[Piece]](6, 9)
      this.board.empty()
      this.pile.empty()
      this.gameStarted = false
    }
  }
  
  /* A helper method that checks whether the given piece is correct in regard with the given symbols.
   * It goes through all the possible positions of a piece by rotating it and returns true if the piece 
   * fits and false if it does not. */
  private def correctPiece(piece: Piece, leftSymbol: Char, upSymbol: Char, p: Int, posN: Int): Boolean = {
    
    /* Is given an integer and rotates a piece this many times. */
    def nRotates(i: Int) = {
      for (j <- 0 until i) {
        piece.rotate()
      }
    }
    
    /* The case where the position of the piece is the same as the position of the place on the board. */
    if (p == posN%2) {
      if (this.correctSides(piece, leftSymbol, upSymbol)) { true } 
      else {
        nRotates(2)
        if (this.correctSides(piece, leftSymbol, upSymbol)) { true } 
        else {
          nRotates(2)
          if (this.correctSides(piece, leftSymbol, upSymbol)) { true } 
          else { false }
        }
      }
      
    /* The case where the position of the piece is not the same as the position of the place on the board. */  
    } else {
      nRotates(1)
      if (this.correctSides(piece, leftSymbol, upSymbol)) { true } 
      else {
        nRotates(2)
        if (this.correctSides(piece, leftSymbol, upSymbol)) { true } 
        else {
          nRotates(2)
          if (this.correctSides(piece, leftSymbol, upSymbol)) { true } 
          else { false }
        }
      }
    }
  }
  
  /* A helper method that matches the position of the piece. It calls the method correctPiece, thus rotating
   * the piece into a correct position if the piece fits. In this case, returns true, otherwise false. */
  private def toCorrectPos(piece: Piece, leftSymbol: Char, upSymbol: Char, p: Int): Boolean = {
    piece.position match {
      case 1 => this.correctPiece(piece, leftSymbol, upSymbol, p, 1)
      case 2 => this.correctPiece(piece, leftSymbol, upSymbol, p, 2)
      case 3 => this.correctPiece(piece, leftSymbol, upSymbol, p, 3)
      case 4 => this.correctPiece(piece, leftSymbol, upSymbol, p, 4)
      case 5 => this.correctPiece(piece, leftSymbol, upSymbol, p, 5)
      case 6 => this.correctPiece(piece, leftSymbol, upSymbol, p, 6)
    }    
  }
  
  /* Tries to solve the game without any given information. */
  def solveGame() = {
    
    /* Stores the used pieces. */
    var pieceStack = Buffer[(Piece, Int)]()
    
    /* Stores the used indices in the pile. */
    var usedIndices = Buffer[Int]()
    
    /* Empties the board if it is not already empty and adds all of the pieces from the board
     * back to the pile. */
    if (!this.board.isEmpty) {
      for {
        i <- 0 until this.board.rows
        j <- 0 until this.board.columns
      } {
        val pieceOnBoard = this.board.getBoard(i)(j).getOrElse(this.board.padPiece)
        if (!pieceOnBoard.samePiece(this.board.padPiece)) {
          this.pile.addPiece(pieceOnBoard)
          this.board.removePiece(pieceOnBoard)
        }
      }
    }

    /* Finds the symbols of the pieces on the left and above of the piece on the given index i. */
    def valuesForSymbols(i: Int) = {
      val leftS = if (i == 0 || i == 5 || i == 12 || i == 19) { 'x' } else { pieceStack(i - 1)._1.convertPos._2 }
      val upS = if (i <= 5 || i == 11) { 'x' } 
        else {
          if ((i >= 6 && i <= 10) || (i >= 19 && i <= 23)) { pieceStack(i - 6)._1.convertPos._4 } 
          else { pieceStack(i - 7)._1.convertPos._4}
        }
      
      (leftS, upS)
    }
    
    /* Finds the value of the position, meaning whether the tip of the triangle is facing up(1) or down(0). */
    def valueForPos(i: Int) = {
      if (i == 0 || i == 2 || i == 4 || i == 20 || i == 22 || (i >= 5 && i <= 18 && i%2 == 1)) 1 else 0
    }
    
    /* Stores information of the symbols, the position of the place, indices, the current piece and the
     * last index in the pile. */
    var symbolL = 'x'
    var symbolU = 'x'
    var position = 1
    var stackIndex = 0
    var pileIndex = 0
    var current = pile.pieceOnIndex(pileIndex)
    var last = 23
    
    /* Tries pieces until it finds the correct solution. In other words, until the size of the stack
     * with added pieces is 24. */
    while (pieceStack.size < 24) {
      
      /* Makes sure that the piece is not already used. */
      if (!usedIndices.contains(pileIndex)) {
        
        /* Tries whether the piece fits in the next position */
        val success = this.toCorrectPos(current.get, symbolL, symbolU, position)
        
        /* If it does, updates the values of the variables. */
        if (success) {
          pieceStack += ((current.get, pileIndex))
          usedIndices += pileIndex
          if (pileIndex == last) { 
            do {
              last -= 1
            } while(usedIndices.contains(last))
          }
          stackIndex += 1
          position = valueForPos(stackIndex)
          symbolL = valuesForSymbols(stackIndex)._1
          symbolU = valuesForSymbols(stackIndex)._2
          pileIndex = 0
          current = this.pile.pieceOnIndex(pileIndex)
        } else {
         
          /* Otherwise, if the index of the piece is the last one in the pile, goes backwards by removing
           * pieces from the stack and updates the values of variables. */
          if (pileIndex == last) {
            var popped = pieceStack.last
            do {
              popped = pieceStack.last
              pieceStack = pieceStack.dropRight(1)
              usedIndices = usedIndices.dropRight(1)
              stackIndex -= 1
              if (popped._2 > last) { last = popped._2 }
            } while (popped._2 >= last)
            position = valueForPos(stackIndex)
            symbolL = valuesForSymbols(stackIndex)._1
            symbolU = valuesForSymbols(stackIndex)._2  
            pileIndex = popped._2 + 1  
            current = this.pile.pieceOnIndex(pileIndex)
          
          /* Else, grows the index by one and updates the current piece. */
          } else {
            pileIndex += 1
            current = this.pile.pieceOnIndex(pileIndex)
          }
        } 
     
        /* If the piece has been used, grows the index by one and updates the current piece. */  
      } else {
        pileIndex += 1
        current = this.pile.pieceOnIndex(pileIndex)
      }
   
    }
    
    /* Adds the pieces on the board and removes them from the pile. */
    for (i <- 0 until pieceStack.length) {
      if (i <= 4) { this.board.addPiece(pieceStack(i)._1, i + 1, 1) }
      if (i >= 5 && i <= 11) { this.board.addPiece(pieceStack(i)._1, i - 4, 2) }
      if (i >= 12 && i <= 18) { this.board.addPiece(pieceStack(i)._1, i - 11, 3) }
      if (i >= 19) { this.board.addPiece(pieceStack(i)._1, i - 18, 4) }
      this.pile.takePiece(pieceStack(i)._1)
    }
    
  }  
  
}