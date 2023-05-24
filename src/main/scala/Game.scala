package xo


import scala.io.StdIn.readLine

  class Game(field:GameField):
  // private val field:GameField
  def over():Boolean = false


  def inputUserAction(currentUser:User):Unit =
    val currentUserName = currentUser.desc
    print(s"Enter coordinates for User$currentUserName :") 
    val coordinates = readLine()
    val c = new CoordinateParser()
    val row:Int = 0
    val column:Int=0
    c.parse(coordinates) match
      case Some(Tuple2(row,column)) => 
        println(s" $row / $column")
        field.fillCell(currentUser,row, column) match
          case false => 
            println("not filled cell. This cell was filled other user.")
            inputUserAction(currentUser)
          case true => println(field)
      case _ => 
        inputUserAction(currentUser)
  
  
