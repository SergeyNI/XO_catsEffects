package xo


import scala.io.StdIn.readLine
import scala.compiletime.ops.boolean

class Game(field:GameField,userX:UserX,userO:UserO):
  // private val field:GameField
  def over(numberAction:Int):Boolean = 
    val res = numberAction >1 & (isWiner(userX) | isWiner(userO) | allCellsFilled())
    if res then
      println(s"game over")
    res


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
  
  def allCellsFilled():Boolean =
    val i = 0
    val res = !anyRowHasEmptyCell(i)
    if res then
      println(s"all cells filled but we have no winners")
    res
  def anyRowHasEmptyCell(i:Int):Boolean =
    val max:Int = field.size()-1
    val row = field.getLine(i,true)
    i match
      case x:Int if x< max =>  rowHasEmptyCell(x) | rowHasEmptyCell(x+1)
      case y:Int if y== max => rowHasEmptyCell(y)
  
  def rowHasEmptyCell(i:Int):Boolean =
    val row = field.getLine(i,true)
    !row.forall(_ != None)
    // val arEmptyCells = for {
    //   c<-row
    //   r = c match
    //   case None => true
    //   case _ =>false
    // } yield r
    // arEmptyCells.filter(c=> c == true).length > 0

  def isWiner(user:User):Boolean =
    val i = 0
    val hasRow = UserHasFilledAnyDirectLine(user,i)
    val hasColumn = UserHasFilledAnyDirectLine(user,i,false)
    val isWiner = hasRow | hasColumn
    if isWiner then
      println(s"${user.userName()} is winner!!")
    isWiner



  def UserHasFilledAnyDirectLine(user:User,i:Int,isRow:Boolean = true):Boolean =
    val max:Int = field.size()-1
    i match
      case x:Int if x< max =>  userHasFilledDirectLine(user,x,isRow) | userHasFilledDirectLine(user,x+1,isRow)
      case y:Int if y== max => userHasFilledDirectLine(user,y,isRow)
  
  def userHasFilledDirectLine(user:User,i:Int, isRow:Boolean = true):Boolean =
    val row = field.getLine(i, isRow)
    row forall (_ == Some(user))
  
