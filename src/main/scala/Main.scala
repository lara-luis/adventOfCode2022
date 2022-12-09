import scala.math.Ordering.Implicits._
import scala.io.Source

object Main extends App {
    var day8 = new Day8
    val lines = Source.fromFile("C:\\git\\adventOfCode2022\\input8.txt").getLines.toList 
    //var res1 = day8.part1(lines)
    val res2 = day8.part2(lines)
    println(res2)
}