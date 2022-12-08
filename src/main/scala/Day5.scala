import scala.collection.immutable._

class Day5 {
    def part1(lines: List[String]): String = {
        var stacks: Array[List[String]] = new Array[List[String]](9)
        var lineNumber = 0 
        var i = 0
        for(line <- lines){
            i = 0
            if(lineNumber < 8){
                var chunks: Array[String] = line.split("(?=(.{4})+$)")
                for (chunk <- chunks){
                    var queue = stacks(i)
                    if (queue == null){
                        queue = List[String]()
                    }
                    
                    if(chunk.trim().nonEmpty){
                        queue = queue :+ chunk
                        stacks(i) = queue
                    }
                    i = i + 1
                }
            }
            lineNumber = lineNumber + 1
            if(line.startsWith("move")){
                var values: List[String] = List.empty
                var lineBlocks = line.split(" ")
                var firstInt = -1
                var secondInt = -1
                var thirdInt = -1
                for(statementPart <- lineBlocks.toList){
                    if(statementPart.forall(Character.isDigit)){
                        if (firstInt == -1) {
                            firstInt = statementPart.toInt
                        } else if (secondInt == -1) {
                            secondInt = statementPart.toInt
                        } else {
                            thirdInt = statementPart.toInt
                        }
                    }
                }

                var sourceQueue = stacks(secondInt.toInt-1)
                var movingBlock = sourceQueue.take(firstInt.toInt)
                sourceQueue = sourceQueue.drop(firstInt.toInt)

                stacks(secondInt.toInt-1) = sourceQueue
                var targetQueue = stacks(thirdInt.toInt-1)
                targetQueue = movingBlock.reverse.concat(targetQueue)
                stacks(thirdInt.toInt-1) = targetQueue
            }
        }
        
        var resultString = ""
        for(q <- stacks){
            resultString = resultString + q.take(1)
        }
        println(resultString)
        resultString
    }

    def part2(lines: List[String]): String = {
        var stacks: Array[List[String]] = new Array[List[String]](9)
        var lineNumber = 0 
        var i = 0
        for(line <- lines){
            i = 0
            if(lineNumber < 8){
                var chunks: Array[String] = line.split("(?=(.{4})+$)")
                for (chunk <- chunks){
                    var queue = stacks(i)
                    if (queue == null){
                        queue = List[String]()
                    }
                    
                    if(chunk.trim().nonEmpty){
                        queue = queue :+ chunk
                        stacks(i) = queue
                    }
                    i = i + 1
                }
            }
            lineNumber = lineNumber + 1
            if(line.startsWith("move")){
                var values: List[String] = List.empty
                var lineBlocks = line.split(" ")
                var firstInt = -1
                var secondInt = -1
                var thirdInt = -1
                for(statementPart <- lineBlocks.toList){
                    if(statementPart.forall(Character.isDigit)){
                        if (firstInt == -1) {
                            firstInt = statementPart.toInt
                        } else if (secondInt == -1) {
                            secondInt = statementPart.toInt
                        } else {
                            thirdInt = statementPart.toInt
                        }
                    }
                }

                var sourceQueue = stacks(secondInt.toInt-1)
                var movingBlock = sourceQueue.take(firstInt.toInt)
                sourceQueue = sourceQueue.drop(firstInt.toInt)

                stacks(secondInt.toInt-1) = sourceQueue
                var targetQueue = stacks(thirdInt.toInt-1)
                targetQueue = movingBlock.concat(targetQueue)
                stacks(thirdInt.toInt-1) = targetQueue
            }
        }
        
        var resultString = ""
        for(q <- stacks){
            resultString = resultString + q.take(1)
        }
        println(resultString)
        resultString    
    }
}