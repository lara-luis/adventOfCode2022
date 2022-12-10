import scala.math.Ordering.Implicits._
import scala.io.Source

object Main extends App {
    var day10 = new Day10
    val lines = Source.fromFile("C:\\git\\adventOfCode2022\\input10.txt").getLines.toList 
    //var res1 = day10.part1(lines)
    val res2 = day10.part2(lines)
    println(res2)
}