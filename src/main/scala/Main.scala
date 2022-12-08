import scala.math.Ordering.Implicits._
import scala.io.Source

object Main extends App {
    var day7 = new Day7
    val lines = Source.fromFile("C:\\git\\adventOfCode2022\\input7.txt").getLines.toList 
    //var res1 = day7.part1(lines)
    val res2 = day7.part2(lines)
    println(res2)
}