package xo
import  cats.effect._
import scala.io.StdIn.readLine
// import xo.Description

object Xo extends IOApp.Simple:
  
  def inputUser(desc:Description):IO[User] =
    for {
      name <- IO(readLine(s"Enter User${{desc}} name:"))
      optUser = UserBuilder(name,desc)
      user: User <- optUser match
        case None => inputUser(desc)
        case Some(value:User) => IO(value)
      
    } yield user
   
  def startGame(f:GameField,userX:User,userO:User):IO[Unit]=
    
    def currentUser(actionNumber:Int):User =
      actionNumber %2 match
        case 0 => userX
        case _: Int => userO
    
    def nextStep(actionNumber:Int,game:Game):Unit =
      game.inputUserAction(currentUser(actionNumber))
      if !game.over(actionNumber) && actionNumber < Int.MaxValue then  nextStep(actionNumber+1,game)
    
    val game = new Game(f,userX,userO)
    nextStep(1,game)

    IO.delay(println(game))
    
  def run: IO[Unit] = 
    val f = new GameField(3)
    
    //val start = inputUser("x").map(userX=>inputUser("o").map(userO=>startGame(f,userX,userO)))
    for{
       
      userX<-inputUser("x")
      userO<-inputUser("o")
      _<- IO(println("Enter number row and column in format 'row column' where you want to set mark. For example: '0 1'  It means first row and second colum"))
      _<-startGame(f,userX,userO)
      _<- IO(println("Game over"))
    } yield ()
    // val userO = inputUser("o")
    
  
    
    // println("Enter number 'row column'") 
    
    


    // val game = new Game(f,userX,userO)
    // println(f)
    
    // def currentUser(numberAction:Int):User =
    //   numberAction %2 match
    //     case 0 => userX
    //     case _: Int => userO
    
    
    // while !game.over(numberAction) && numberAction < Int.MaxValue do
    //   numberAction += 1
    //   val currUser = currentUser(numberAction)
    //   game.inputUserAction(currUser)
  // run(List[String].apply())

      