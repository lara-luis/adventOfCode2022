import scala.math.Ordering.Implicits._
import scala.io.Source

object Main extends App {
    var day3 = new Day3
    val lines = Source.fromFile("C:\\git\\adventOfCode2022\\input3.txt").getLines.toList 
    var res1 = day3.part1(lines)
    val res2 = day3.part2(lines)
    println(res1)
}