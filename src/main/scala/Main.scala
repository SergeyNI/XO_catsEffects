package xo
import  cats.effect._
import scala.io.StdIn.readLine
// import cats.instances.unit

object Xo extends IOApp.Simple:
  
  def inputUserName(desc:Description):IO[User] =
    for {
      name <- IO(readLine(s"Enter user ${{desc}} name:>"))
      optUser = UserBuilder(name,desc)
      user: User <- optUser match
        case None => inputUserName(desc)
        case Some(value:User) => IO(value)
      
    } yield user
   
  def startGame(f:GameField,userX:User,userO:User):IO[Unit]=
    
    def currentUser(actionNumber:Int):User =
      actionNumber %2 match
        case 0 => userX
        case _: Int => userO
    
    def nextStep(actionNumber:Int,game:Game):IO[Unit] =
      val curUser = currentUser(actionNumber)
      for {
        _<- game.inputUserAction(curUser)
        result = game.over(curUser)
        _<-if !result._1 && actionNumber < Int.MaxValue then 
            nextStep(actionNumber+1,game) 
          else
            result._2 match
              case false => 
                IO.println("all cells filled, but we have no winners")
              case true =>
                IO.println(s"${curUser.name} is WINNER!!")
      } yield ()
    
    val game = new Game(f,userX,userO)
    nextStep(1,game)
 
  def run: IO[Unit] = 
    val f = new GameField(3)
    for{
      userX<-inputUserName("X")
      userO<-inputUserName("O")
      _<- IO(println("Enter number row and column in format 'row column' where you want to set mark. For example: '0 1'  It means first row and second colum"))
      _<-startGame(f,userX,userO)
      _<- IO(println("Game over"))
    } yield ()