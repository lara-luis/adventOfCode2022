import scala.math.Ordering.Implicits._
import scala.io.Source

object Main extends App {
    var day15 = new Day15
    val lines = Source.fromFile("C:\\git\\adventOfCode2022\\input15.txt").getLines.toList 
    //var res1 = day15.part1(lines)
    val res2 = day15.part2(lines)
    println(res2)
}