package xo


import scala.io.StdIn.readLine
import cats.effect.{IO}

class Game(field:GameField,userX:User,userO:User):
  val printGameOver:IO[Unit] = IO(println(s"game over"))

  def over(currentUser:User):Tuple2[Boolean,Boolean] = 
    val  currentUserWinner = isWiner(currentUser)
    val over = currentUserWinner | allCellsFilled()
    Tuple2(over,currentUserWinner)

  def inputUserAction(currentUser:User):IO[Unit] =
    for {
      coordinatesString <- IO.print(s"${currentUser.name} enter cell coordinates:")*>IO.readLine
      coordinates <- new CoordinateParser().parse(coordinatesString)
      _<- coordinates match
          case Some(Tuple2(row,column)) => 
            field.fillCell(currentUser,row, column) match
              case false => 
                IO.println("cell not filled, because this cell was filled other user.")*>inputUserAction(currentUser)
              case true => IO.println(field)
          case _ => 
            inputUserAction(currentUser)
    } yield()
  
  def allCellsFilled():Boolean =  rowHasNotEmptyCell(0)
  
  def rowHasNotEmptyCell(i:Int):Boolean = 
    val max:Int = field.size()-1
    val row = field.getLine(i,true)
    i match
      case x:Int if x< max =>  
        val row = field.getLine(i,true)
        row.forall(_ != None) & rowHasNotEmptyCell(i+1)
      case y:Int if y== max => field.getLine(i,true).forall(_ != None)

 
  def isWiner(user:User):Boolean =
    val i = 0
    val hasRow = UserHasFilledAnyDirectLine(user,i)
    val hasColumn = UserHasFilledAnyDirectLine(user,i,false)
    val hasAnyCrossLine = userHasFilledAnyCrossLine(user:User)

    hasRow | hasColumn | hasAnyCrossLine
  
  def userHasFilledAnyCrossLine(user:User):Boolean =
    val cl1 = field.getCrossLine(true)
    val cl2 = field.getCrossLine(false)
    cl1.forall(_ == Some(user)) | cl2.forall(_ == Some(user))
    
  def UserHasFilledAnyDirectLine(user:User,i:Int,isRow:Boolean = true):Boolean =
    val max:Int = field.size()-1
    i match
      case x:Int if x< max =>  userHasFilledDirectLine(user,x,isRow) | userHasFilledDirectLine(user,x+1,isRow)
      case y:Int if y== max => userHasFilledDirectLine(user,y,isRow)
  
  def userHasFilledDirectLine(user:User,i:Int, isRow:Boolean = true):Boolean =
    val row = field.getLine(i, isRow)
    row.forall(_ == Some(user))
  
