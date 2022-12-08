import scala.math.Ordering.Implicits._
import scala.io.Source

object Main extends App {
    var day5 = new Day5
    val lines = Source.fromFile("C:\\git\\adventOfCode2022\\input5.txt").getLines.toList 
    //var res1 = day5.part1(lines)
    val res2 = day5.part2(lines)
    println(res2)
}