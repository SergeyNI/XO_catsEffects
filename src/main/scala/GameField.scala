package xo

import scala.annotation.meta.field


type Cell = Option[User]
type Line = Array[Cell]

class GameField(length:Int):
  private val cells: Array[Line] =  {
    def newLine():Line = new Array[Cell](this.length)
    val newArray = Array.ofDim[Cell](length, length)
    // val line:Line = new Array[Cell](this.length)
    val field = new Array[Line](length)
    
    Range(0,length-1).inclusive.foreach(i=>
      val line = newLine()
      Range(0,length-1).inclusive.foreach(i=>line(i) = None)
      field(i) = line)
    field
  }

  override def toString(): String =
    val l = cells.length
    def op(cell:Cell,s:String):String =
      val res = cell match {
        case Some(c:User) => c.desc
        case null => "_"
        case None => "_"
      }
      s" $res $s"
    def foldline(s:String,line:Line):String =
      val r = line.foldRight[String]("\n")(op)
      s"$s $r"
    cells.foldLeft[String]("")(foldline)
  
  def fillCell(user:User,row:Int, column:Int):Boolean =
    val value = cells(row)(column)
    println(s"! $value")
    println(s"! $row / $column")
    value match
      case None =>
        cells(row)(column) = Some(user)
        true
      case Some(other_user) => false
    
