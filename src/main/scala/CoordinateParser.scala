package xo
import cats.effect.{IO}
class CoordinateParser {

  def parse(str:String):IO[Option[Tuple2[Int,Int]]]=
    
    val arrayofChars = str.split(" ")
    val res = arrayofChars.length match
      case 2 =>
        try {
          val firstEl = arrayofChars(0).toInt
          val secondEl = arrayofChars(1).toInt
          Some(Tuple2(firstEl,secondEl))
        } catch {
          case ex: java.lang.NumberFormatException => None 
        }        
      case x: Int => None
      
    IO(res)
}
