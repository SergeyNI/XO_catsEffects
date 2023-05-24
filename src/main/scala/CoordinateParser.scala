package xo
class CoordinateParser {
  def parse(str:String):Option[Tuple2[Int,Int]]=
    val arrayofChars = str.split(" ")
    arrayofChars.length match
      case 2 => Some(Tuple2(arrayofChars(0).toInt,arrayofChars(1).toInt))
      case x: Int => 
        println("not correct coordinates")
        println(x)
        None
    
}
