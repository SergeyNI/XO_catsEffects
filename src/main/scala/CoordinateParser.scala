package xo
import cats.effect.{IO}
class CoordinateParser {
  def parse(str:String):IO[Option[Tuple2[Int,Int]]]=
    val arrayofChars = str.split(" ")
    val res = arrayofChars.length match
      case 2 => Some(Tuple2(arrayofChars(0).toInt,arrayofChars(1).toInt))
      case x: Int => 
        println("not correct coordinates")
        println(x)
        None
    IO(res)
}
