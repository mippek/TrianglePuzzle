package puzzle.UI

import puzzle._  

import scala.swing._
import scala.swing.event._
import scala.swing.BorderPanel.Position._
import javax.swing.ImageIcon
import javax.swing.JLayeredPane
import javax.imageio.ImageIO
import java.io.File
import java.awt.image.BufferedImage
import java.awt.{ Image, Color, Graphics2D , Toolkit }


object PuzzleGUI extends SimpleSwingApplication {
  
  /* The size of the window. */
  val screenSize = Toolkit.getDefaultToolkit().getScreenSize()
  val width = screenSize.getWidth().toInt
  val height = screenSize.getHeight().toInt
  val windowSize = new Dimension(width, height)
  
  /* The main frame. */
  def top = new MainFrame {
    title = "The Triangle Puzzle"
    minimumSize = windowSize
    maximumSize = windowSize
    peer.setLocationRelativeTo(null)
    contents = new MainPanel
  }
  
  /* The game and the pile and the board of the game. */
  val game = new Game
  val board = game.board
  val pile = game.pile
  
  /* Stores coordinates when the mouse is moved and listened to. */
  var movedPiece: Option[Piece] = None
  var movedCoords = (0, 0)  //(x, y)
  var startCoords = (0, 0)  //(x, y)
  
  /* The dimensions and coords of the board background. */
  val w1 = 5 * width / 9
  val h1 = 3 * height / 4
  val coords1 = (3 * width / 8, height / 8)
  
  /* The dimensions and coords of the pile background. */
  val w2 = 7 * width / 32
  val coords2 = (3 * width / 32, height / 8)
  val h2 = 3 * height / 4
  
  /* Coordinates for the corners of the board. */
  val one = (coords1._1 + 50 , coords1._2 + h1 / 2)  //(x, y)
  val four = (coords1._1 + w1 - 50, coords1._2 + h1 / 2)
  
  /* Side and diameter(height) of a triangle piece. */
  val side = (four._1 - one._1) / 4.0
  val diameter = side * 1.0 / math.sqrt(2)
  
  val two = (one._1 + side.toInt, one._2 - (2 * diameter).toInt)
  val three = (two._1 + (2 * side).toInt, two._2)  
  val five = (three._1, one._2 + (2 * diameter).toInt) 
  val six = (two._1, five._2)
  val oneTwo = (one._1 + ((two._1 - one._1) / 2.0).toInt, two._2 + ((one._2 - two._2) / 2.0).toInt)
  val twoThree = (two._1 + side.toInt, two._2)
  
   /* The scaled images for the background of the game and the puzzle pieces. */
  val originalBg = ImageIO.read(new File("puzzlebackground.jpg"))
  val bgImg = originalBg.getScaledInstance(width, height, Image.SCALE_DEFAULT)
  val originalPieceUp = ImageIO.read(new File("puzzlepiece.png"))
  val pieceUpImg = originalPieceUp.getScaledInstance(side.toInt, diameter.toInt, Image.SCALE_DEFAULT)
  val originalPieceDown = ImageIO.read(new File("puzzlepiecedown.png"))
  val pieceDownImg = originalPieceDown.getScaledInstance(side.toInt, diameter.toInt, Image.SCALE_DEFAULT)
  val originalHint = ImageIO.read(new File("hintText.png"))
  val hintImg = originalHint.getScaledInstance(2 * w2 / 5, 2 * h2 / 32 , Image.SCALE_DEFAULT)
  
  
  /* Draws the background for the game and the game board and the pieces for the pile and 
   * the board as well as the moving piece. */
  def onPaint(g: Graphics2D) = {
    g.drawImage(bgImg, 0, 0, null)  
    g.setColor(new Color(255, 250, 250))
    g.fillRect(coords1._1, coords1._2, w1, h1)
    g.fillRect(coords2._1, coords2._2, w2, h2) 
    g.drawImage(hintImg, coords2._1 + w2 /2, coords2._2 + h2 - 3 * h2 / 32, null)
    
    /* Draws the lines for the board. */
    g.setColor(Color.black)
    g.drawLine(one._1, one._2, two._1, two._2)
    g.drawLine(one._1, one._2, four._1, four._2)
    g.drawLine(one._1, one._2, six._1, six._2) 
    g.drawLine(two._1, two._2, three._1, three._2)
    g.drawLine(two._1, two._2, five._1, five._2) 
    g.drawLine(three._1, three._2, four._1, four._2)
    g.drawLine(three._1, three._2, six._1, six._2)  
    g.drawLine(four._1, four._2, five._1, five._2) 
    g.drawLine(five._1, five._2, six._1, six._2)  
    g.drawLine(oneTwo._1, oneTwo._2, oneTwo._1 + (3 * side).toInt, oneTwo._2)
    g.drawLine(oneTwo._1, oneTwo._2 + (2 * diameter).toInt, oneTwo._1 + (3 * side).toInt, oneTwo._2 + (2 * diameter).toInt)  
    g.drawLine(oneTwo._1, oneTwo._2, twoThree._1, twoThree._2 + (4 * diameter).toInt)
    g.drawLine(twoThree._1, twoThree._2, oneTwo._1 + (3 * side).toInt, oneTwo._2 + (2 * diameter).toInt) 
    g.drawLine(twoThree._1, twoThree._2, oneTwo._1, oneTwo._2 + (2 * diameter).toInt)
    g.drawLine(twoThree._1, twoThree._2 + (4 * diameter).toInt, oneTwo._1 + (3 * side).toInt, oneTwo._2)
    
    /* Draws the piece on top of the pile and the symbols on its sides */
    val currentPiece = pile.currentPiece.getOrElse(board.padPiece)
    val font2 = new Font("Arial", java.awt.Font.BOLD, w2 / 24/*16*/)
    g.setColor(new Color(255, 250, 250))
    g.setFont(font2)
    if (!currentPiece.samePiece(board.padPiece)) { 
      val symbols = currentPiece.convertPos
      val y = coords2._2 + (h2 / 2)
      val x = coords2._1 + (w2 / 5) + 5
      if (currentPiece.position%2 == 1) {
        g.drawImage(pieceUpImg, x, y, null)
        g.drawString(symbols._1.toString, x + (side / 4).toInt + 20, y + (diameter / 2).toInt)  //left
        g.drawString(symbols._2.toString, x + (side * 3 / 4).toInt - 30, y + (diameter / 2).toInt)  //right
        g.drawString(symbols._4.toString, x + (side / 2).toInt, y + diameter.toInt - 15)  //bottom
      } else {
        g.drawImage(pieceDownImg, x, y, null)
        g.drawString(symbols._1.toString, x + (side / 4).toInt + 15, y + (diameter / 2).toInt)  //left
        g.drawString(symbols._2.toString, x + (side * 3 / 4).toInt - 20, y + (diameter / 2).toInt)  //right
        g.drawString(symbols._3.toString, x + (side / 2).toInt, y + 25)  //top
      }
    }
    
    /* Draws the pieces that are currently on the board. */
    for {
      i <- 0 until board.rows
      j <- 0 until board.columns
    } {
      val x11 = two._1 - (side / 2).toInt
      val y11 = two._2
      val x21 = one._1
      val y21 = one._2 - diameter.toInt
      val addedX = (side / 2)
      val addedY = diameter
      val piece = board.getBoard(i)(j).getOrElse(board.padPiece)
      
      if (!piece.samePiece(board.padPiece)) {
        val symbols = piece.convertPos
        if (piece.position%2 == 1) {
          if (i == 1 || i == 4) {
            g.drawImage(pieceUpImg, x11 + ((j - 2) * addedX).toInt, y11 + ((i - 1) * addedY).toInt, null)
            g.drawString(symbols._1.toString, x11 + ((j - 2) * addedX).toInt + (side / 4).toInt + 20, y11 + ((i - 1) * addedY).toInt + (diameter / 2).toInt)  //left
            g.drawString(symbols._2.toString, x11 + ((j - 2) * addedX).toInt + (side * 3 / 4).toInt - 30, y11 + ((i - 1) * addedY).toInt + (diameter / 2).toInt)  //right
            g.drawString(symbols._4.toString, x11 + ((j - 2) * addedX).toInt + (side / 2).toInt, y11 + ((i - 1) * addedY).toInt + diameter.toInt - 15)  //bottom
          }
          if (i == 2 || i == 3) {
            g.drawImage(pieceUpImg, x21 + ((j - 1) * addedX).toInt, y21 + ((i - 2) * addedY).toInt, null)
            g.drawString(symbols._1.toString, x21 + ((j - 1) * addedX).toInt + (side / 4).toInt + 20, y21 + ((i - 2) * addedY).toInt + (diameter / 2).toInt)  //left
            g.drawString(symbols._2.toString, x21 + ((j - 1) * addedX).toInt + (side * 3 / 4).toInt - 30, y21 + ((i - 2) * addedY).toInt + (diameter / 2).toInt)  //right
            g.drawString(symbols._4.toString, x21 + ((j - 1) * addedX).toInt + (side / 2).toInt, y21 + ((i - 2) * addedY).toInt + diameter.toInt - 15)  //bottom
          }  
        } else {
          if (i == 1 || i == 4) {
            g.drawImage(pieceDownImg, x11 + ((j - 2) * addedX).toInt, y11 + ((i - 1) * addedY).toInt, null)
            g.drawString(symbols._1.toString, x11 + ((j - 2) * addedX).toInt + (side / 4).toInt + 15, y11 + ((i - 1) * addedY).toInt + (diameter / 2).toInt)  //left
            g.drawString(symbols._2.toString, x11 + ((j - 2) * addedX).toInt + (side * 3 / 4).toInt - 20, y11 + ((i - 1) * addedY).toInt + (diameter / 2).toInt)  //right
            g.drawString(symbols._3.toString, x11 + ((j - 2) * addedX).toInt + (side / 2).toInt, y11 + ((i - 1) * addedY).toInt + 25)  //top
          }
          if (i == 2 || i == 3) {
            g.drawImage(pieceDownImg, x21 + ((j - 1) * addedX).toInt, y21 + ((i - 2) * addedY).toInt, null)
            g.drawString(symbols._1.toString, x21 + ((j - 1) * addedX).toInt + (side / 4).toInt + 15, y21 + ((i - 2) * addedY).toInt + (diameter / 2).toInt)  //left
            g.drawString(symbols._2.toString, x21 + ((j - 1) * addedX).toInt + (side * 3 / 4).toInt - 20, y21 + ((i - 2) * addedY).toInt + (diameter / 2).toInt)  //right
            g.drawString(symbols._3.toString, x21 + ((j - 1) * addedX).toInt + (side / 2).toInt, y21 + ((i - 2) * addedY).toInt + 25)  //top
          }
        }
      }  
    }
    
    /* Draws the piece that is being moved. */
    val moved = movedPiece.getOrElse(board.padPiece)
    if (!moved.samePiece(board.padPiece)) { 
      val symbols = moved.convertPos
      if (moved.position%2 == 1) {
        g.drawImage(pieceUpImg, movedCoords._1, movedCoords._2, null)
        g.drawString(symbols._1.toString, movedCoords._1 + (side / 4).toInt + 20, movedCoords._2 + (diameter / 2).toInt)  //left
        g.drawString(symbols._2.toString, movedCoords._1 + (side * 3 / 4).toInt - 30, movedCoords._2 + (diameter / 2).toInt)  //right
        g.drawString(symbols._4.toString, movedCoords._1 + (side / 2).toInt, movedCoords._2 + diameter.toInt - 15)  //bottom
      } else {
        g.drawImage(pieceDownImg, movedCoords._1, movedCoords._2, null)
        g.drawString(symbols._1.toString, movedCoords._1 + (side / 4).toInt + 15, movedCoords._2 + (diameter / 2).toInt)  //left
        g.drawString(symbols._2.toString, movedCoords._1 + (side * 3 / 4).toInt - 20, movedCoords._2 + (diameter / 2).toInt)  //right
        g.drawString(symbols._3.toString, movedCoords._1 + (side / 2).toInt, movedCoords._2 + 25)  //top
      }
    }
  
  }
  
  /* Converts the coords on the screen given in pixels to match the indexes in the board array. */
  def convertCoords(x: Int, y: Int): (Int, Int) = {
    val x11 = two._1 - (side / 2).toInt
    val y11 = two._2
    val x11Side = x11 + (side / 4).toInt
    val y11Side = y11 + (diameter / 2).toInt
    val addedX = (side / 2)
    val addedY = (diameter / 2)
    
    if (y >= y11Side && y <= (y11Side + addedY.toInt))
        if (x >= x11Side && x <= (x11Side + addedX.toInt)) (1, 2) else //(y, x)
          if (x >= (x11Side + (2 * addedX).toInt) && x <= (x11Side + (3 * addedX).toInt)) (1, 4) else
            if (x >= (x11Side + (4 * addedX).toInt) && x <= (x11Side + (5 * addedX).toInt)) (1, 6) else (-1, -1)
            
    else if (y >= y11 && y <= y11Side)
        if (x >= (x11Side + addedX.toInt) && x <= (x11Side + (2 * addedX).toInt)) (1, 3) else
          if (x >= (x11Side + (3 * addedX).toInt) && x <= (x11Side + (4 * addedX).toInt)) (1, 5) else (-1, -1)
          
    else if (y >= (y11Side + (2 * addedY).toInt) && y <= (y11Side + (3 * addedY). toInt))
        if (x >= (x11Side - addedX.toInt) && x <= x11Side) (2, 1) else
          if (x >= (x11Side + addedX.toInt) && x <= (x11Side + (2 * addedX).toInt)) (2, 3) else
            if (x >= (x11Side + (3 * addedX).toInt) && x <= (x11Side + (4 * addedX).toInt)) (2, 5) else 
              if (x >= (x11Side + (5 * addedX).toInt) && x <= (x11Side + (6 * addedX).toInt)) (2, 7) else (-1, -1)
              
    else if (y >= (y11Side + addedY.toInt) && y <= (y11Side + (2 * addedY).toInt)) 
        if (x >= x11Side && x <= (x11Side + addedX.toInt)) (2, 2) else
          if (x >= (x11Side + (2 * addedX).toInt) && x <= (x11Side + (3 * addedX).toInt)) (2, 4) else
            if (x >= (x11Side + (4 * addedX).toInt) && x <= (x11Side + (5 * addedX).toInt)) (2, 6) else  (-1, -1)
            
    else if (y >= (y11Side + (3 * addedY).toInt) && y <= (y11Side + (4 * addedY).toInt))
        if (x >= (x11Side - addedX.toInt) && x <= x11Side) (3, 1) else
          if (x >= (x11Side + addedX.toInt) && x <= (x11Side + (2 * addedX).toInt)) (3, 3) else
            if (x >= (x11Side + (3 * addedX).toInt) && x <= (x11Side + (4 * addedX).toInt)) (3, 5) else
              if (x >= (x11Side + (5 * addedX).toInt) && x <= (x11Side + (6 * addedX).toInt)) (3, 7) else (-1, -1)
              
    else if (y >= (y11Side + (4 * addedY).toInt) && y <= (y11Side + (5 * addedY).toInt))
        if (x >= x11Side && x <= (x11Side + addedX.toInt)) (3, 2) else
          if (x >= (x11Side + (2 * addedX).toInt) && x <= (x11Side + (3 * addedX).toInt)) (3, 4) else
            if (x >= (x11Side + (4 * addedX).toInt) && x <= (x11Side + (5 * addedX).toInt)) (3, 6) else (-1, -1)
            
    else if (y >= (y11Side + (5 * addedY).toInt) && y <= (y11Side + (6 * addedY).toInt)) 
        if (x >= x11Side && x <= (x11Side + addedX.toInt)) (4, 2) else
          if (x >= (x11Side + (2 * addedX).toInt) && x <= (x11Side + (3 * addedX).toInt)) (4, 4) else
            if (x >= (x11Side + (4 * addedX).toInt) && x <= (x11Side + (5 * addedX).toInt)) (4, 6) else (-1, -1)
            
    else if (y >= (y11Side + (6 * addedY).toInt) && y <= (y11Side + (7 * addedY).toInt)) 
        if (x >= (x11Side + addedX.toInt) && x <= (x11Side + (2 * addedX).toInt)) (4, 3) else
          if (x >= (x11Side + (3 * addedX).toInt) && x <= (x11Side + (4 * addedX).toInt)) (4, 5) else (-1, -1)
          
    else (-1, -1)   
  }
  
  /* Is called when a mouse is clicked. If this occurs on the pile or on a piece on the board,
   * the clicked piece is rotated.  */
  def onClick(x: Int, y: Int) = {
    val pileY = coords2._2 + (h2 / 2)
    val pileX = coords2._1 + (w2 / 5) + 5
    
    /* Rotates the piece in the pile */
    if ((x >= pileX && x <= (pileX + side)) && (y >= pileY && y <= (pileY + diameter))) {
      val current = pile.currentPiece.getOrElse(board.padPiece)
      if (!current.samePiece(board.padPiece)) {
        current.rotate
      }
    }
    
    /* Calls the method getHelp if the mouse is clicked on the hint button. */
    val hintY = coords2._2 + h2 - 3 * h2 / 32
    val hintX = coords2._1 + w2 / 2
    
    if ((x >= hintX && x <= (hintX + 2 * w2 / 5)) && (y >= hintY && y <= (hintY + 2 * h2 / 32))) { game.getHelp() }
    
    /* Rotates the piece on the board. */
    val converted = convertCoords(x, y)
    if (converted._1 != -1) {
      val current = board.getBoard(converted._1)(converted._2).getOrElse(board.padPiece)
      if (!current.samePiece(board.padPiece)) {
        current.rotate()
        current.rotate()
      }
    }
  }
  
  /* Is called when a mouse is moved. Updates the value of movedPiece, the piece that is 
   * currently moved, if needed. */
  def onMove(x: Int, y: Int) = {
    val pileY = coords2._2 + (h2 / 2)
    val pileX = coords2._1 + (w2 / 5) + 5
    
    /* The moved piece is from the pile. */
    if ((startCoords._1 >= pileX && startCoords._1 <= (pileX + side)) && (startCoords._2 >= pileY && startCoords._2 <= (pileY + diameter))) {  
      val current = pile.currentPiece.getOrElse(board.padPiece)
      if (!current.samePiece(board.padPiece) && movedPiece == None) {
        pile.takePiece(current)
        movedPiece = Some(current)
      } 
    
    /* The moved piece is from the board. */  
    } else {
      val convertedStart = convertCoords(startCoords._1, startCoords._2)
      if (convertedStart._1 != -1) {
        val current = board.getBoard(convertedStart._1)(convertedStart._2).getOrElse(board.padPiece)
        if (!current.samePiece(board.padPiece) && movedPiece == None) {
          board.removePiece(current)
          movedPiece = Some(current)
        }
      }
    }
  }
  
  /* Is called when the mouse is released. */
  def onRelease(x: Int, y: Int): Unit = {
    val pileY = coords2._2 + (h2 / 2)
    val pileX = coords2._1 + (w2 / 5) + 5
    val current = movedPiece.getOrElse(board.padPiece)
    val converted = convertCoords(x, y)
    
    /* The start coords are on top of the pile. If the end coords are wrong, the piece is
     * added back to the pile */
    if ((startCoords._1 >= pileX && startCoords._1 <= (pileX + side)) && (startCoords._2 >= pileY && startCoords._2 <= (pileY + diameter))) {   
      if (converted._1 != -1) {
        if (!current.samePiece(board.padPiece) && movedPiece != None) {
          if (converted._1 == 1 || converted._1 == 4) {
            val success = board.addPiece(current, converted._2 - 1, converted._1)
            if (!success) { pile.addPiece(current) }
          }
          if (converted._1 == 2 || converted._1 == 3) {
            val success = board.addPiece(current, converted._2, converted._1)
            if (!success) { pile.addPiece(current) }
          }
        }
      } else {
        if (!current.samePiece(board.padPiece)) { pile.addPiece(current) }
      }
      
    /* The start coords are on top of the board. If the end coords are wrong, the piece is
     * added back to the position where it started. */  
    } else {
      val convertedStart = convertCoords(startCoords._1, startCoords._2)
      if (convertedStart._1 != -1) {
        if (converted._1 != -1) {
          if (!current.samePiece(board.padPiece) && movedPiece != None) {
            if (converted._1 == 1 || converted._1 == 4) {
              val success = board.addPiece(current, converted._2 - 1, converted._1)
              if (!success) {
                if (convertedStart._1 == 1 || convertedStart._1 == 4) { board.addPiece(current, convertedStart._2 - 1, convertedStart._1) }
                if (convertedStart._1 == 2 || convertedStart._1 == 3) { board.addPiece(current, convertedStart._2, convertedStart._1) }
              }
            }
            if (converted._1 == 2 || converted._1 == 3) {
              val success = board.addPiece(current, converted._2, converted._1)
              if (!success) {
                if (convertedStart._1 == 1 || convertedStart._1 == 4) { board.addPiece(current, convertedStart._2 - 1, convertedStart._1) }
                if (convertedStart._1 == 2 || convertedStart._1 == 3) { board.addPiece(current, convertedStart._2, convertedStart._1) }
              }
            }
          }
        } else {
          if ((x >= pileX && x <= (pileX + side)) && (y >= pileY && y <= (pileY + diameter))) {
            if (current.left != 'x') { pile.addPiece(current) }
          } else {
            if (!current.samePiece(board.padPiece)) {
              if (convertedStart._1 == 1 || convertedStart._1 == 4) { board.addPiece(current, convertedStart._2 - 1, convertedStart._1) }
              if (convertedStart._1 == 2 || convertedStart._1 == 3) { board.addPiece(current, convertedStart._2, convertedStart._1) }
            }
          }
        } 
      }
    }
  }
  
  /* The class for the main panel. It contains all of the buttons and listens to them and the mouse. */
  class MainPanel extends BoxPanel(Orientation.Vertical) {
    
    /* Draws the image. */
    override def paintComponent(g: Graphics2D) = {
      onPaint(g)
    }
    
    val button1Dim = new Dimension(w2 / 3, h2 / 16) 
    val button2Dim = new Dimension(w2 / 5, h2 / 16) 
    val buttonFont = new Font("Arial", java.awt.Font.PLAIN, w2 / 28)
    
    /* The buttons are created. */
    val newGame = new Button {
      font = buttonFont
      text = "New game"
      minimumSize = button1Dim
      preferredSize = button1Dim
      maximumSize = button1Dim
    }
    val continue = new Button {
      font = buttonFont
      text = "Continue game"
      minimumSize = button1Dim
      preferredSize = button1Dim
      maximumSize = button1Dim
    }
    val solve = new Button {
      font = buttonFont
      text = "Let me solve it"
      minimumSize = button1Dim
      preferredSize = button1Dim
      maximumSize = button1Dim
    }
    val save = new Button {
      font = buttonFont
      text = "Save and close"
      minimumSize = button1Dim
      preferredSize = button1Dim
      maximumSize = button1Dim
    }
    val next = new Button {
      text = "🠊"
      minimumSize = button2Dim
      preferredSize = button2Dim
      maximumSize = button2Dim
    }
    val previous = new Button {
      text = "🠈"
      minimumSize = button2Dim
      preferredSize = button2Dim
      maximumSize = button2Dim
    }
    
    /* Creates the structure of the main frame by combining panels. */
    val buttons11 = new BoxPanel(Orientation.Horizontal) {
      contents += Swing.HStrut(3 * width / 32 + 3 * width / 100)
      contents += newGame
      contents += Swing.HStrut(2 * width / 100)
      contents += continue
      background = new Color(0, 0, 0, 0)
    }
    val buttons12 = new BoxPanel(Orientation.Horizontal) {
      contents += Swing.HStrut(3 * width / 32 + 3 * width / 100)
      contents += solve
      contents += Swing.HStrut(2 * width / 100)
      contents += save
      background = new Color(0, 0, 0, 0)
    }
    val buttons1 = new BoxPanel(Orientation.Vertical) {
      contents += buttons11
      contents += Swing.VStrut(-1 * height / 3 + 3 * height / 100)
      contents += buttons12
      background = new Color(0, 0, 0, 0)
    }
    val buttons2 = new BoxPanel(Orientation.Horizontal) {
      contents += Swing.HStrut(4 * width / 32 + 2 * width / 100)
      contents += previous
      contents += Swing.HStrut(3 * width / 100)
      contents += next
      background = new Color(0, 0, 0, 0)
    }
    
    /* Adds the panels to the contents of the main panel. */
    contents += new BorderPanel {
      add(buttons1, BorderPanel.Position.West)
      background = new Color(0, 0, 0, 0)
    }
    contents += Swing.VStrut(10)
    contents += Swing.VStrut(10)
    contents += new BorderPanel {
      add(buttons2, BorderPanel.Position.West)
      background = new Color(0, 0, 0, 0)
    }
    
    /* Pop-up windows. */
    def startMessage() {
      val text = "You started a new game. Here are some instructions: \n" + 
      "The computer has generated a new solution for the game and the pile on the left contains all of the pieces. \n" + 
      "Your goal is to solve the puzzle by matching the sides of pieces: match all of the uppercase letters to the corresponding \n" +
      "lowercase letters and vice versa. \n" +
      "You can click on the arrows to flip through the pile and click on the piece on top to rotate it. A piece has six different positions. \n" + 
      "To place pieces on the board grab the piece from the pile and drag it to the correct place. \n" +
      "Pieces can be also moved on the board and from the board back to the pile in which case the piece goes to the back of the pile. \n" +
      "During the game there can be contradictory solutions on the board but a piece must be in the same position (tip up or tip down) \n" +
      "as its place on the board. Because of this, the piece is automatically rotated to the nearest suitable position in case it isn't in it yet. \n" +
      "To rotate a piece on the board just click on it. In this case the piece rotates two steps to remain in the correct position. \n" +
      "You can save your game and continue it later by pressing on the 'Save and close' button. When you then wish to continue the game \n" +
      "open the app again and press on 'Continue game'. This always opens the last game you've saved. \n" + 
      "If you are having trouble solving the puzzle, press on 'Let me solve it' and the computer will try and solve the puzzle for you. \n" +
      "You also have the 'HINT?' button. Click on the hint and all of the false pieces will be deleted from the board and the correct \n" +
      "pieces rotated to correct positions. It also adds three new pieces correctly on the board or just one if there are less than three \n" +
      "pieces left in the pile. \n\n" +
      "Now, LET'S PLAY!!"
      Dialog.showMessage(contents(1), text, title="Game started")
    }
    def continueMessage() {
      val text = "The last saved game was continued"
      Dialog.showMessage(contents(1), text, title="Game continued")
    }
    def solveMessage() {
      val text = "The computer solved the puzzle for you"
      Dialog.showMessage(contents(1), text, title="Puzzle solved")
      game.endGame()
      repaint()
    }
    def saveMessage() {
      val text = "The game was successfully saved. \n Do you want to close the app?"
      val res = Dialog.showConfirmation(contents(1), text, optionType=Dialog.Options.YesNo, title="Game saved")
      if (res == Dialog.Result.Yes) { sys.exit(0) }
    }
    def endMessage() {
      val text = "Congratulations! You solved the puzzle successfully!"
      Dialog.showMessage(contents(1), text, title="Puzzle solved")
      game.endGame()
      repaint()
    }
    
    /* Listens to the buttons and the mouse. At first, only listens to the buttons new game
     * and continue. */
    listenTo(newGame)
    listenTo(continue)
    
    /* Reacts to the buttons and mouse. */
    reactions += {
      case ButtonClicked(`newGame`) =>
        game.startGame()
        listenTo(solve)
        listenTo(save)
        listenTo(next)
        listenTo(previous)
        listenTo(mouse.clicks)
        listenTo(mouse.moves)
        deafTo(newGame)
        deafTo(continue)
        repaint()
        startMessage()
      case ButtonClicked(`continue`) => 
        val gameSituation = FileOperations.readFromFile("gameSituation.txt")
        game.continueGame(gameSituation._1, gameSituation._2, gameSituation._3)
        listenTo(solve)
        listenTo(save)
        listenTo(next)
        listenTo(previous)
        listenTo(mouse.clicks)
        listenTo(mouse.moves)
        deafTo(newGame)
        deafTo(continue)
        repaint()
        continueMessage()
      case ButtonClicked(`solve`) =>
        game.solveGame()
        repaint()
        if (game.gameOver) {
          listenTo(newGame)
          listenTo(continue)
          deafTo(solve)
          deafTo(save)
          deafTo(next)
          deafTo(previous)
          deafTo(mouse.clicks)
          deafTo(mouse.moves)
          solveMessage()
        }
      case ButtonClicked(`save`) =>
        FileOperations.writeToFile("gameSituation.txt", board.getBoard, pile.piecePile, game.solution)
        saveMessage()
      case ButtonClicked(`next`) => 
        pile.flipRight
        repaint()
      case ButtonClicked(`previous`) =>
        pile.flipLeft
        repaint()
      case MouseClicked(_, p, _, _, _) =>
        onClick(p.x, p.y)
        repaint()
        if (game.gameOver) {
          listenTo(newGame)
          listenTo(continue)
          deafTo(solve)
          deafTo(save)
          deafTo(next)
          deafTo(previous)
          deafTo(mouse.clicks)
          deafTo(mouse.moves)
          endMessage()
        }
      case MousePressed(_, p, _, _, _) => 
        startCoords = (p.x, p.y)
      case MouseDragged(_, p, _) => 
        onMove(p.x, p.y)
        movedCoords = (p.x - (side / 2).toInt, p.y - (diameter / 2).toInt)
        repaint()
      case MouseReleased(_, p, _, _, _) =>  
        onRelease(p.x, p.y)
        repaint()
        movedPiece = None
        movedCoords = (0, 0)
        startCoords = (0, 0)
        if (game.gameOver) {
          listenTo(newGame)
          listenTo(continue)
          deafTo(solve)
          deafTo(save)
          deafTo(next)
          deafTo(previous)
          deafTo(mouse.clicks)
          deafTo(mouse.moves)
          endMessage()
        }
    }  
  }
  
}