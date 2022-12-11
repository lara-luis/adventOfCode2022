import scala.math.Ordering.Implicits._
import scala.io.Source

object Main extends App {
    var day11 = new Day11
    val lines = Source.fromFile("C:\\git\\adventOfCode2022\\input11.txt").getLines.toList 
    //var res1 = day11.part1(lines)
    val res2 = day11.part2(lines)
    println(res2)
}