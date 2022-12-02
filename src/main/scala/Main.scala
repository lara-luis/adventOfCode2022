import scala.math.Ordering.Implicits._
import scala.io.Source

object Main extends App {
    var day2 = new Day2
    val lines = Source.fromFile("C:\\git\\adventOfCode2022\\input2.txt").getLines.toList 
    var res1 = day2.part1(lines)
    val res2 = day2.part2(lines)
    println(res2)
}