import scala.math.Ordering.Implicits._
import scala.io.Source

object Main extends App {
    var day1 = new Day1
    val lines = Source.fromFile("C:\\git\\adventOfCode2022\\input1.txt").getLines.toList 
    var res1 = day1.part1(lines)
    val res2 = day1.part2(lines)
    println(res2)
}

