package xo
import scala.io.StdIn.readLine

type Desc = "_" | "x" |"o"

trait User(name:String):
  require(name.length()>3)
  def desc:Desc


case class UserX(name:String) extends User(name:String):
  def desc:Desc = "x"
// object UserX:
//   def apply(name:String):Option[UserX] =
//     try{
//       val user = UserX(name)
//       Some(user)
      
//     } catch {
//       case exception: IllegalArgumentException => None
//     }
    
    
case class UserO(name:String) extends User(name:String):
  def desc:Desc = "o"

class UserBuilder(name:String,classUser:Class[_])
  //def newUser(name:String,classUser:Class[_]): Option[Any]

object UserBuilder:
  def apply(name:String,classUser:Class[_]):Option[Any]=
    try {
      val c = classUser.getConstructor(classOf[String])
      val user = c.newInstance(name)
      Some(user)
      } catch {
        case exception: IllegalArgumentException => None
        case e => 
          println("not entered name. Enter name with length greater than 3 character")
          None
      }
    
def inputUser(t:String, userClass:Class[_]):Any =
  print(s"Enter User$t name:") 
  val nameX = readLine()
  val userX = UserBuilder(nameX,userClass)
  userX match
    case None => inputUser(t,userClass)
    case Some(u) => u
