package puzzle

import scala.collection.mutable.Buffer

class PileOfPieces {
  
  /* The pile where the pieces are stored. It is a private val to not be able to change it from 
   * other classes. */
  private val pile = Buffer[Piece]()
  
  /*The current piece in the pile. The current piece is the piece that is on top of the pile. If there
   * are no pieces in the pile, it has the value None. */
  private var current: Option[Piece] = None
  
  /* Returns the piece on top of the pile. */
  def currentPiece = this.current
  
  /* Returns the pile of pieces as an immutable vector. */
  def piecePile = this.pile.toVector
  
  /* Checks whether the pile contains a certain piece or not. */
  def contains(p: Piece): Boolean = this.pile.exists( _.samePiece(p) ) 
  
  /* Checks whether the pile is empty. */
  def isEmpty: Boolean = this.pile.isEmpty
  
  /* Returns the size of the pile. */
  def size = this.pile.size
  
  /* Returns the piece from the pile on the given index. The piece is given in an option, and, if the pile is
   * empty or the index is greater than the size of the pile, None is returned. */
  def pieceOnIndex(i: Int): Option[Piece] = if (this.size - 1 >= i) Some(this.pile(i)) else None
  
  /* Adds a piece into the pile. A piece is always added at the end of the pile. A piece is added only
   * if the pile does not already contain the piece in question. The piece added to the pile becomes
   * the current piece in the pile. */
  def addPiece(p: Piece) = {
    if (!this.pile.exists( _.samePiece(p) )) {
      this.pile += p
      this.current = Some(p)
    }
  }
  
  /* Takes a piece from the pile. A piece can be taken only if the pile contains the piece in question.
   * If the piece that is taken from the pile is the last one and the pile becomes empty, the current piece
   * is updated to be None. If the piece being taken from the pile happens to be the current piece, the 
   * next piece on the right becomes the current piece. */
  def takePiece(p: Piece) = {
    if (this.pile.exists( _.samePiece(p) )) {
      if (p.samePiece(this.current.getOrElse(new Piece('x', 'x', 'x', 1)))) {
        if (this.size == 1) {
          this.current = None
        } else {
          this.flipRight
        }
      }
      this.pile -= p
    }
  }
  
  /* Flips through the pile to the left. */
  def flipLeft() = {
    if (this.size > 1) {
      val i = this.pile.indexWhere( _.samePiece(this.current.get) )
      if (i == 0) {
        this.current = Some(this.pile(this.size - 1))
      } else {
        this.current = Some(this.pile(i - 1))
      }    
    } 
  }
  
  /* Flips through the pile to the right. */
  def flipRight() = {
    if (this.size > 1) {
      val i = this.pile.indexWhere( _.samePiece(this.current.get))
      if (i == this.size - 1) {
        this.current = Some(this.pile(0))
      } else {
        this.current = Some(this.pile(i + 1))
      }
    }
  }
  
  /* Empties the entire pile of all pieces. */
  def empty() = {
    if (!this.isEmpty) {
      for (i <- 0 until this.size) {
        this.takePiece(pile(0))
      }
    }
  }
  
}