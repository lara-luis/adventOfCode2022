import scala.collection.mutable.ListBuffer

class Day3 {
    val lowerAlphabet = "abcdefghijklmnopqrstuvwxyz"
    val upperAlphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    private def GetIntFromChar(c: String): Int = {
        if(c.toLowerCase() == c){
            lowerAlphabet.indexOf(c)+1
        }else {
            upperAlphabet.indexOf(c)+1+26
        }
    }

    private def GetLineValue(line: String): Int = {
        val length = line.length
        val firstHalf = line.substring(0,(length/2)).toList
        val secondHalf = line.substring(length/2, length).toList
        val commonEl = firstHalf.intersect(secondHalf)
        
        GetIntFromChar(commonEl.head.toString())
    }

    def part1(lines: List[String]): Int = {
        var sum = 0
        for (l <- lines) {
            sum += GetLineValue(l)
        }
        sum
    }

    private def GetTripletValue(lines: ListBuffer[String]): Int = {
        val commonEl = (lines(0).intersect(lines(1))).intersect(lines(2))
        GetIntFromChar(commonEl.head.toString())
    }

    def part2(lines: List[String]): Int = {
        var sum = 0
        var groupLinesCount = 0;
        var threeLines = ListBuffer[String]()
        for (l <- lines) {
            if (groupLinesCount == 2){
                threeLines.append(l)
                sum += GetTripletValue(threeLines)
                groupLinesCount = 0
                threeLines = ListBuffer[String]()
            } else {
                threeLines.append(l)
                groupLinesCount +=1
            }            
        }
        sum
    }
}