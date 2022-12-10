import scala.collection.mutable._

class Day9 {
    def UpdateT(posH: (Int,Int), posT: (Int,Int)): (Int,Int) = {
        if(posH._1 == posT._1 && posH._2 == posT._2){
            return posT
        }
        else if(posH._1 == posT._1 && posH._2 == posT._2 - 2){
            return (posT._1, posT._2 - 1)
        }
        else if(posH._1 == posT._1 && posH._2 == posT._2 + 2){
            return (posT._1, posT._2 + 1)
        } 
        else if(posH._1 == posT._1 - 2 && posH._2 == posT._2){ 
            return (posT._1 - 1, posT._2)
        }
        else if(posH._1 == posT._1 + 2 && posH._2 == posT._2){
            return (posT._1 + 1, posT._2)
        }
        else if(posH._1 != posT._1 && posH._2 != posT._2) {
            if((posH._1-posT._1).abs == 1 && (posH._2-posT._2).abs == 1){
                return posT
            } else {
                var newX = posT._1
                var newY = posT._2
                if((posH._1 - posT._1).abs == 2){
                    if(posH._1 > posT._1){
                        newX = posT._1 + 1
                    } else {
                        newX = posT._1 - 1
                    }
                    if(posH._2 > posT._2){
                        newY = posT._2 + 1
                    } else {
                        newY = posT._2 - 1
                    }
                } else if((posH._2 - posT._2).abs == 2){
                    if(posH._2 > posT._2){
                        newY = posT._2 + 1
                    } else {
                        newY = posT._2 - 1
                    }
                    if(posH._1 > posT._1){
                        newX = posT._1 + 1
                    } else {
                        newX = posT._1 - 1
                    }
                }
                return (newX, newY)
            }
        }
        posT
    }

    def part1(lines: List[String]): Int = {
        var directions: List[(String,Int)] = List()
        var visitedPositions: Set[(Int,Int)] = Set()
        var currH: (Int,Int) = (0,0)
        var currT: (Int,Int) = (0,0)
        
        for (line <- lines){
            var lineArray = line.split(" ")
            var direction = lineArray(0)
            var moves = lineArray(1).toInt 
            directions = directions :+ ((direction, moves))
        }

        visitedPositions.add(currT);
        for(d <- directions){
            var currMoveProgress = d._2
            while(currMoveProgress > 0){
                if(d._1 == "R"){
                    currH = (currH._1 + 1, currH._2)
                } else if(d._1 == "L"){
                    currH = (currH._1 - 1, currH._2)
                } else if(d._1 == "U"){
                    currH = (currH._1, currH._2 + 1)
                } else if(d._1 == "D"){
                    currH = (currH._1, currH._2 - 1)
                }
                
                var newCoord = UpdateT(currH, currT)
                currT = newCoord
                visitedPositions.add(newCoord);
                currMoveProgress -= 1
            }
        }
        visitedPositions.size
    }
    
    def part2(lines: List[String]): Int = {
        var directions: List[(String,Int)] = List()
        var visitedPositions: Set[(Int,Int)] = Set()
        var currH: (Int,Int) = (0,0)
        var curr1: (Int,Int) = (0,0)
        var curr2: (Int,Int) = (0,0)
        var curr3: (Int,Int) = (0,0)
        var curr4: (Int,Int) = (0,0)
        var curr5: (Int,Int) = (0,0)
        var curr6: (Int,Int) = (0,0)
        var curr7: (Int,Int) = (0,0)
        var curr8: (Int,Int) = (0,0)
        var curr9: (Int,Int) = (0,0)
        
        for (line <- lines){
            var lineArray = line.split(" ")
            var direction = lineArray(0)
            var moves = lineArray(1).toInt 
            directions = directions :+ ((direction, moves))
        }

        visitedPositions.add(curr9);
        for(d <- directions){
            var currMoveProgress = d._2
            while(currMoveProgress > 0){
                if(d._1 == "R"){
                    currH = (currH._1 + 1, currH._2)
                } else if(d._1 == "L"){
                    currH = (currH._1 - 1, currH._2)
                } else if(d._1 == "U"){
                    currH = (currH._1, currH._2 + 1)
                } else if(d._1 == "D"){
                    currH = (currH._1, currH._2 - 1)
                }
                
                var newCoord = UpdateT(currH, curr1)
                curr1 = newCoord

                newCoord = UpdateT(curr1, curr2)
                curr2 = newCoord

                newCoord = UpdateT(curr2, curr3)
                curr3 = newCoord

                newCoord = UpdateT(curr3, curr4)
                curr4 = newCoord

                newCoord = UpdateT(curr4, curr5)
                curr5 = newCoord

                newCoord = UpdateT(curr5, curr6)
                curr6 = newCoord

                newCoord = UpdateT(curr6, curr7)
                curr7 = newCoord

                newCoord = UpdateT(curr7, curr8)
                curr8 = newCoord

                newCoord = UpdateT(curr8, curr9)
                curr9 = newCoord

                visitedPositions.add(curr9);
                currMoveProgress -= 1
            }
        }
        visitedPositions.size
    }
}