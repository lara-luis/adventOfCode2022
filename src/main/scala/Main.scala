import scala.math.Ordering.Implicits._
import scala.io.Source

object Main extends App {
    var day12 = new Day12
    val lines = Source.fromFile("C:\\git\\adventOfCode2022\\input12.txt").getLines.toList 
    //var res1 = day12.part1(lines)
    val res2 = day12.part2(lines)
    println(res2)
}