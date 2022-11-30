//> using lib "com.lihaoyi::os-lib:0.8.1"
import os._
import scala.util.matching.Regex

val data = os.read.lines( os.pwd / "input" )

enum direction:
  case forward, down, up


val pattern = """(\w+) (\d+)""".r
                                                                                                                                                                                                            
def move(d: direction, i: Int, depth: Int, position: Int) =
  d match {
    case direction.up => if (depth < i ) (0, position) else (depth-i, position)
    case direction.down => (depth + i , position)
    case direction.forward => (depth, position + i)
  }

val res1 = data.map(str => str match {
  case pattern(dir, amount) => (direction.valueOf(dir), amount.toInt)
  case _ => throw new Exception(s"BIG ERROR ${str} ")
}  )


val res2 = r.foldLeft((0,0))( (a, e) => move(e._1, e._2, a._1, a._2) )

val result = res2._1 * res2._2