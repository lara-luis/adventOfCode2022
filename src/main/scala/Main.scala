import scala.math.Ordering.Implicits._
import scala.io.Source

object Main extends App {
    var day4 = new Day4
    val lines = Source.fromFile("C:\\git\\adventOfCode2022\\input4.txt").getLines.toList 
    var res1 = day4.part1(lines)
    val res2 = day4.part2(lines)
    println(res2)
}