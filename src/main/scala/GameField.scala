package xo

import scala.annotation.meta.field


type Cell = Option[User]
type Line = Array[Cell]

class GameField(length:Int):
  
  private val cells: Array[Line] =  {
    def newLine():Line = new Array[Cell](this.length)
    val newArray = Array.ofDim[Cell](length, length)
    val field = new Array[Line](length)
    val range = 0 until length
    range foreach(i =>
      val line = newLine()
      Range(0,length-1).inclusive.foreach(i=>line(i) = None)
      field(i) = line)
    field
  }
  def size():Int = length

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
    value match
      case None =>
        cells(row)(column) = Some(user)
        true
      case Some(other_user) => false
  
  def getRow(i:Int):Line = cells(i)
  
  def getColumn(i:Int):Line = 
    val column = for {
      row<-cells
    } yield row(i)
    column
  
  def getLine(i:Int, isRow:Boolean = true):Line =
    isRow match
      case true => getRow(i)
      case _=> getColumn(i)
  def getCrossLine(stright:Boolean):Line =
    val line = new Line(3)
    
    stright match
      case true => 
        for 
          i<-length-1 to 0 by -1
          j =length-i-1
        do 
          line(j) =  cells(i)(j)
      
      case false =>
        for i <- 0 until length
        do line(i) =  cells(i)(i) 
        
      line


    

