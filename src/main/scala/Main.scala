import scala.math.Ordering.Implicits._
import scala.io.Source

object Main extends App {
    var day6 = new Day6
    val lines = Source.fromFile("C:\\git\\adventOfCode2022\\input6.txt").getLines.toList 
    var res1 = day6.part1(lines)
    val res2 = day6.part2(lines)
    println(res2)
}