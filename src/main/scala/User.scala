package xo
import scala.io.StdIn.readLine

type Description = "_" | "X" |"O"

case class User(name:String,desc:Description):
  require(name.length()>3)



class UserBuilder(name:String,desc:Description) 

object UserBuilder:
  def apply(name:String,desc:Description):Option[User]=
    try {
      Some(User(name,desc))
      } catch {
        case _ => None
      }
    
