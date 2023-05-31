package xo

@main def hello: Unit = 
  val f = new GameField(3)

  val userX = inputUser("X",classOf[UserX]).asInstanceOf[UserX]
  val userO = inputUser("O",classOf[UserO]).asInstanceOf[UserO]
  println(f)
  println("Enter number 'row column'")
  
  var numberAction = 1;
  // val userX = new UserX("XXXX")
  // val userO = new UserO("OOO")
  // f.fillCell(userX,0,0)
  // f.fillCell(userO,1,0)
  // f.fillCell(userO,1,1)
  // f.fillCell(userO,1,2)
  val game = new Game(f,userX,userO)
  println(f)
  
  def currentUser(numberAction:Int):User =
    numberAction %2 match
      case 0 => userX
      case _: Int => userO
  
  
  while !game.over(numberAction) && numberAction < Int.MaxValue do
    numberAction += 1
    val currUser = currentUser(numberAction)
    game.inputUserAction(currUser)
    
    