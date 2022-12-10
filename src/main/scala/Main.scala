import scala.math.Ordering.Implicits._
import scala.io.Source

object Main extends App {
    var day9 = new Day9
    val lines = Source.fromFile("C:\\git\\adventOfCode2022\\input9.txt").getLines.toList 
    //var res1 = day9.part1(lines)
    val res2 = day9.part2(lines)
    println(res2)
}