package puzzle

import java.io._
import scala.collection.mutable.Buffer

object FileOperations {
  
  /* Takes as parameters a filename, an array of the situation on the board, a vector of the pile, and
   * an array of the solution for the game. Writes the information from these collections to a file. If
   * a file with the file name does not exist, it creates a new file with that name. */
  def writeToFile(fileName: String, gameSituation: Array[Array[Option[Piece]]], pile: Vector[Piece], gameSolution: Array[Array[Option[Piece]]]) = {
  
    try {
      val fileOut = new FileWriter(fileName)
      val linesOut = new BufferedWriter(fileOut)
      
      try {
        
        /* A helper method for writing the information of an array. Writes each row of the array table on
         * separate rows. */
        def writeBoardInfo(arr: Array[Array[Option[Piece]]]) = {
          for (i <- 0 until arr.length) {
            var string = ""
            for (j <- 0 until arr(i).length) {
              val onePiece = arr(i)(j).getOrElse(new Piece('O', 'O', 'O', 1))
              if (onePiece.left != 'O' && onePiece.left != 'x') {
                string += onePiece.left
                string += onePiece.bottom
                string += onePiece.right
                string += onePiece.position.toString
                string += onePiece.getCoords._1.toString
                string += onePiece.getCoords._2.toString
                string += " "
              }
            }
            linesOut.write(string)
            linesOut.newLine()
          }
        }
        
        linesOut.write("#GAME")
        linesOut.newLine()
        linesOut.newLine()
        
        /* Writes the information for the situation on the board. */
        linesOut.write("#BOARD")
        linesOut.newLine()
        writeBoardInfo(gameSituation)
        
        /* Writes all of the pieces in the pile vector on separate rows. */
        linesOut.write("#PILE")
        linesOut.newLine()
        for (i <- 0 until pile.length) {
          var string = ""
          val onePiece = pile(i)
          string += onePiece.left
          string += onePiece.bottom
          string += onePiece.right
          string += onePiece.position.toString
          linesOut.write(string)
          linesOut.newLine()
        }
        linesOut.newLine()
        
        /* Writes the information for the solution of the game. */
        linesOut.write("#SOLUTION")
        linesOut.newLine()
        writeBoardInfo(gameSolution)
        
      } finally {
        linesOut.close()
        fileOut.close()
      }
      
    } catch {
      /* Catches exceptions in writing to the file. */
      case notFound: FileNotFoundException => println("File not found")
      case e: IOException => println("Writing finished with error")
    }
  }
  
  /* Takes as a parameter a name of a file and reads the information from that file. If the file does
   * not exist, returns an exception. If the reading was successfull, returns a tuple with an array
   * for the board, a buffer for the pile and an array for the solution. */
  def readFromFile(fileName: String): (Array[Array[Option[Piece]]], Buffer[Piece], Array[Array[Option[Piece]]]) = {
    try {
      val fileIn = new FileReader(fileName)
      val linesIn = new BufferedReader(fileIn)
      
      try {
        /* Variables that keep track of the information read. */
        var currentLine = linesIn.readLine()
        var header = ""
        var subHeader = ""
        var arr = Array.fill[Option[Piece]](6, 9)(Some(new Piece('x', 'x', 'x', 1)))
        val pile = Buffer[Piece]()
        var sol = Array.fill[Option[Piece]](6, 9)(Some(new Piece('x', 'x', 'x', 1)))
        
        if (currentLine.toLowerCase startsWith "#game") {
          header = "#game"
        }
        
        /* A helper method that gets a string as a parameter and determines which piece the string is. */
        def piece(pieceInfo: String) = {
          val l = pieceInfo.head
          val b = pieceInfo.drop(1).head
          val r = pieceInfo.drop(2).head
          val p = pieceInfo.drop(3).head
    
          new Piece(l, r, b, p - '0')
        }
      
        /* A helper method that gets a string as a parameter and determines the coordinates of a piece. */
        def piecePlace(placeInfo: String) = {
          val r = placeInfo.head
          val c = placeInfo.drop(1).head
          val row = r - '0'
          var col = c - '0'
          if (r == 1 || r == 4) { col += 1 }
          (row, col)
        }
        
        /* A helper method for reading the information of an array from a file. */
        def readBoardInfo(s: String, a: Array[Array[Option[Piece]]]) = {
          val info = s.split(" ")
          for (p <- 0 until info.length) {
            val pie = piece(info(p).take(4).trim)
            val pla = piecePlace(info(p).drop(4).trim)
            if (pla._1 == 1 || pla._1 == 4) {
              a(pla._1)(pla._2 + 1) = Some(pie)
            } else {
              if (pla._1 == 2 || pla._1 == 3) {
                a(pla._1)(pla._2) = Some(pie)
              }
            }
          }
        }
        
        while({currentLine = linesIn.readLine(); currentLine != null}) {
          if (!currentLine.equals("") && currentLine != null) {
            currentLine = currentLine.trim
          }
          if (currentLine.toLowerCase startsWith "#game") {
            header = "#game"
          }
          if (currentLine.toLowerCase startsWith "#board") {
            subHeader = "#board"
          }
          if (currentLine.toLowerCase startsWith "#pile") {
            subHeader = "#pile"
          }
          if (currentLine.toLowerCase startsWith "#solution") {
            subHeader = "#solution"
          }
          
          /* Reads the information for the board. */
          if (header == "#game" && subHeader == "#board" && !(currentLine.toLowerCase startsWith "#") && !currentLine.equals("")) {
            readBoardInfo(currentLine, arr)
          }
          
          /* Reads the information for the pile. */
          if (header == "#game" && subHeader == "#pile" && !(currentLine.toLowerCase startsWith "#") && !currentLine.equals("")) {
            val info = currentLine.trim
            val pie = piece(info)
            pile += pie
          }
          
          /* Reads the information for the solution. */
          if (header == "#game" && subHeader == "#solution" && !(currentLine.toLowerCase startsWith "#") && !currentLine.equals("")) {
            readBoardInfo(currentLine, sol)
          }
         
        }
       
       /* Returns the board, pile and solution. */
       (arr, pile, sol)
        
      } finally {
        linesIn.close()
        fileIn.close()
      }
      
    } catch {
      /* Catches exceptions in reading from the file. */
      case notFound: FileNotFoundException => {
        println("File not found")
        (Array(Array(None)), Buffer(), Array(Array(None)))
      }
      case e: IOException => {
        println("Reading finished with error")
        (Array(Array(None)), Buffer(), Array(Array(None)))
      }
    }
  }
}