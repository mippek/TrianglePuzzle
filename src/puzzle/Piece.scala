package puzzle

/* When a new instance of a piece is created, its left side, right side and bottom side are given 
 * as well as its position. The three sides are vals and can not be changed. They are the actual left,
 * right and bottom side of the piece in position 1 even though the position would not currently be 1. */
class Piece(val left: Char, val right: Char, val bottom: Char, private var pos: Int) {
  
  /* Stores the coordinates of a piece in a var to be able to change them. The coordinated
   * are stored in an option since a piece does not always have coordinates, e.g. when it is in the pile. */
  private var coords: Option[(Int, Int)] = None
  
  /* Returns the position of the piece. */
  def position = this.pos
  
  /* Returns the coordinated of the piece. If the piece does not have coordinates, returns (-1, -1). */
  def getCoords = this.coords.getOrElse(-1, -1)
  
  /* Adds coordinates to the piece. */
  def addCoords(y: Int, x: Int) = {
    this.coords = Some(y, x)
  }
  
  /* Removes the coordinates of the piece. */
  def removeCoords() = {
    this.coords = None
  }
  
  /* Rotates the piece. A piece has six different positions. One rotation grows the position by one, e.g., 
   * from 1 to 2 unless the position is 6. In this case, the position returns to 1 when rotated. */
  def rotate() = {
    if (this.pos == 6) { this.pos = 1 } else { this.pos += 1 } 
  }
  
  /* Converts the position of the piece to a tuple that shows in which order the sides of the piece actually
   * are after the piece has possibly been rotated. A tuple has four chars. The order in the tuple is 
   * (left, right, up, down). A char 'O' represents the tip of the triangle. */
  def convertPos = {
    this.pos match {
      case 1 => (left, right, 'O', bottom)  //(left, right, up, down)
      case 3 => (bottom, left, 'O', right)
      case 5 => (right, bottom, 'O', left)
      case 2 => (bottom, right, left, 'O')
      case 4 => (right, left, bottom, 'O')
      case 6 => (left, bottom, right, 'O') 
    }
  }
  
  /* Checks whether this piece is the same piece as the piece given as a parameter. Two pieces are the same
   * if their left sides have the same symbol, their right sides the same symbol and their bottom sides the same 
   * symbol. */
  def samePiece(p: Piece) = {
    this.left == p.left && this.right == p.right && this.bottom == p.bottom
  }
  
  /* A string to help debugging */
  override def toString = "left: " + this.left + ", right: " + this.right + ", bottom: " + this.bottom + ", pos: " + this.pos
  
}