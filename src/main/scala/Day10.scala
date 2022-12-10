import java.io.File
import java.io.PrintWriter

class Day10 {
    def ADDX_CYCLE_TIME = 2
    
    def part1(lines: List[String]): Int = {
        var result = 1
        var sumOfResults = 0
        var currCyle = 1
        var instructions: List[(String, Int)] = List()

        for(l <- lines){
            var tmp = l.split(" ")
            var value = ""
            if (tmp(0) == "noop"){
                value = "-1"
            } else {
                value = tmp(1)
            }
            instructions = instructions :+ (tmp(0), value.toInt)
        }

        for(l <- instructions){
            if(l._1.startsWith("noop")){
                // nienté
                currCyle += 1
                
                if(currCyle == 20 || currCyle == 60 || currCyle == 100 || currCyle == 140 ||
                    currCyle == 180 || currCyle == 220){
                    println(currCyle + " " + result + " = " + (currCyle*result))
                    sumOfResults += (currCyle*result)
                }
            } else {
                currCyle += 1

                if(currCyle == 20 || currCyle == 60 || currCyle == 100 || currCyle == 140 ||
                    currCyle == 180 || currCyle == 220){
                    println(currCyle + " " + result + " = " + (currCyle*result))
                    sumOfResults += (currCyle*result)
                }

                currCyle += 1
                result += l._2

                if(currCyle == 20 || currCyle == 60 || currCyle == 100 || currCyle == 140 ||
                    currCyle == 180 || currCyle == 220){
                    println(currCyle + " " + result + " = " + (currCyle*result))
                    sumOfResults += (currCyle*result)
                }
            }
        }
        sumOfResults
    }

    def part2(lines: List[String]): Int = {
        var result = 1
        var currCyle = 1
        var instructions: List[(String, Int)] = List()
        var crtPos = 0

        var line = "........................................"

        for(l <- lines){
            var tmp = l.split(" ")
            var value = ""
            if (tmp(0) == "noop"){
                value = "-1"
            } else {
                value = tmp(1)
            }
            instructions = instructions :+ (tmp(0), value.toInt)
        }

        var xPosPerCycle: Map[Int /*cycle*/, Int /*X value*/] = Map.empty
        xPosPerCycle = xPosPerCycle + (1 -> 1)

        for(l <- instructions){
            if(l._1.startsWith("noop")){
                // nienté
                currCyle += 1

                xPosPerCycle = xPosPerCycle + (currCyle -> result)
            } else {
                currCyle += 1

                xPosPerCycle = xPosPerCycle + (currCyle -> result)

                currCyle += 1
                result += l._2

                xPosPerCycle = xPosPerCycle + (currCyle -> result)
            }
        }

        for(cycle <- 1 to 240){ // cycles go advancing...
            var xPos = xPosPerCycle.get(cycle)
            var xPosInt = xPos.get

            if(crtPos == xPosInt || crtPos == (xPosInt-1) || crtPos == (xPosInt+1)){
                val sb = new StringBuilder(line)
                sb.setCharAt(crtPos, '#');
                line = sb.mkString
            }

            if(crtPos == 39){
                crtPos = 0
                println(line)
                line = "........................................"
            } else {
                crtPos += 1
            }
        }
        0
    }
}