package puzzle

class Board {
  
  /* The board that stores all of the pieces. The pieces are wrapped in an option since there might be
   * no pieces on a certain spot on the board. In this case contains None instead. */
  private val board = Array.fill[Option[Piece]](6, 9)(None)
  
  /* Stores information of all of the possible positions on the board. The integer 0 represents
   * padding. Integers 1 and 2 represent places where a piece can go: 1 represents the tip of the triangle
   * facing up and 2 the tip facing down. */
  val placesOnBoard = Array.ofDim[Int](6, 9)
  
  /* Adds the correct places to the array containing the piece places. */
  private def addPlaces() = {
    this.placesOnBoard(0) = Array(0, 0, 0, 0, 0, 0, 0, 0, 0)
    this.placesOnBoard(1) = Array(0, 0, 1, 2, 1, 2, 1, 0, 0)
    this.placesOnBoard(2) = Array(0, 1, 2, 1, 2, 1, 2, 1, 0)
    this.placesOnBoard(3) = Array(0, 2, 1, 2, 1, 2, 1, 2, 0)
    this.placesOnBoard(4) = Array(0, 0, 2, 1, 2, 1, 2, 0, 0)
    this.placesOnBoard(5) = Array(0, 0, 0, 0, 0, 0, 0, 0, 0)
  }
  
  /* Padding piece. */
  val padPiece = new Piece('x', 'x', 'x', 1)
  
  /* Creates padding around the board since the board is not square. Padding makes it easier to handle
   * the separate positions on the board without having to separately handle the positions on the edges.
   * The padding consists of pieces Piece('x', 'x', 'x', 1), where all of the sides have the char 'x'. */
  private def init() = {
    for {
      i <- 0 until this.board.length
      j <- 0 until this.board(i).length
    } {
      if (i == 0 || i == 5) { this.board(i)(j) = Some(this.padPiece) }
      if ((i == 1 || i == 4) && (j <= 1 || j >= 7)) { this.board(i)(j) = Some(this.padPiece) }
      if ((i == 2 || i == 3) && (j == 0 || j == 8)) { this.board(i)(j) = Some(this.padPiece) }
    }
    this.addPlaces()
    
  }
  
  this.init()
  
  /* Returns a clone of the board. With it pieces on the board can be accessed but the board
   * can not be changed. */
  def getBoard = this.board.clone
  
  /* Return the size of the board, the number of rows and columns. */
  def rows = this.board.length
  
  def columns = this.board(0).length
  
  /* Returns a piece in an option from the board on some coordinates. If the given coords are
   * not on the board or there is no piece on these coords, the method returns None.  */
  def pieceOnCoords(x: Int, y: Int): Option[Piece] = {
    if (y >= 1 && y <= 4) {
      if ((y == 1 || y == 4) && (x >= 1 && x <= 5)) this.board(y)(x + 1)
      else if ((y == 2 || y == 3) && (x >= 1 && x <= 7)) this.board(y)(x)
      else None
    } else {
      None
    }
  }
  
  /* Adds a piece on the board. The method returns a Boolean value that tells whether the piece has
   * successfully been added or not. A piece can only be added on the board meaning that the y
   * value can vary between 1 and 4, and the x value between 1 and 5 if y=1 or y=4, or between 1 and 7
   * if y=2 or y=3. Otherwise the method returns false. The method also returns false if a piece is 
   * added to a position that already contains a piece. In each position the triangles can be either 
   * tip upwards or tip downwards depending on the position. The method rotates the pieces to the 
   * closest suitable position if they are already not correctly positioned. */
  def addPiece(p: Piece, x: Int, y: Int): Boolean = {
    if (y >= 1 && y <= 4) {
      if ((y == 1 || y == 4) && (x >= 1 && x <= 5) && this.board(y)(x + 1) == None) {
        if (y == 1 && (x == 1 || x == 3 || x == 5) && p.position%2 == 0) p.rotate() 
        if (y == 1 && (x == 2 || x == 4) && p.position%2 == 1) p.rotate()
        if (y == 4 && (x == 1 || x == 3 || x == 5) && p.position%2 == 1) p.rotate()
        if (y == 4 && (x == 2 || x == 4) && p.position%2 == 0) p.rotate()
        this.board(y)(x + 1) = Some(p)
        p.addCoords(y, x)
        true
      } else {
        if ((y == 2 || y == 3 ) && (x >= 1 && x <= 7) && this.board(y)(x) == None) {
          if (y == 2 && (x == 1 || x == 3 || x == 5 || x == 7) && p.position%2 == 0) p.rotate()
          if (y == 2 && (x == 2 || x == 4 || x == 6) && p.position%2 == 1) p.rotate()
          if (y == 3 && (x == 1 || x == 3 || x == 5 || x == 7) && p.position%2 == 1) p.rotate()
          if (y == 3 && (x == 2 || x == 4 || x == 6) && p.position%2 == 0) p.rotate() 
          this.board(y)(x) = Some(p)
          p.addCoords(y, x)
          true
        } else { false }
      }
    } else { false }
  }
  
  /* Removes a piece from the board. Only a piece can be removed that is on the board. The method returns
   * a Boolean value that tells whether the piece has successfully been removed or not. */
  def removePiece(p: Piece): Boolean = {
    val n = 
      for {
        i <- 0 until this.board.length
        j <- 0 until this.board(i).length
        if (this.board(i)(j) == Some(p))
      } yield (i, j)
    val coords = n.headOption.getOrElse(-1, -1)
    
    if (coords != (-1, -1) && !p.samePiece(this.padPiece)) {
      this.board(coords._1)(coords._2) = None
      p.removeCoords()
      true
    } else { false }  
  }
  
  /* Returns true if the board is full, meaning there is a piece in every position. Otherwise
   * returns false. */
  def isFull = this.board.forall( _.forall( _ != None ) )
  
  /* Returns true if the board is empty, meaning the only pieces on the board are padding pieces. */
  def isEmpty = this.board.forall( _.forall( _.getOrElse(this.padPiece).samePiece(this.padPiece) ) )
  
  /* Empties the board from all pieces. After the board has been emptied, the method also calls
   * the method init() to create padding around the board again. */
  def empty() = {
    for {
      i <- 0 until this.board.length
      j <- 0 until this.board(i).length
    } {
      this.board(i)(j) = None
      this.init()
    }
  }
  
}