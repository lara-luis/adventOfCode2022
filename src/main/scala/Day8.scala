import scala.collection.mutable._

class Day8 {
    var ROW_NUM = 99
    var COL_NUM = 99

    def GetColumn(j: Int, matrix: Array[Array[Int]]): Array[Int] = {
        var col : Array[Int] = Array()
        for (l <- matrix){
            col = col :+ l(j)
        }
        col
    }

    def IsVisible(el: Int, i: Int, j: Int, matrix: Array[Array[Int]]): Boolean = {
        var isVisible = false
        if (i == 0 || j == 0 || i == ROW_NUM-1 || j == COL_NUM-1){
            return true
        } 

        var treeLine =  matrix(i)
        var leftTrees = treeLine.slice(0,j)
        var rightTrees = treeLine.slice(j+1, treeLine.size)
        isVisible = leftTrees.forall(_ < el) || rightTrees.forall(_ < el)
        if (!isVisible){
            var treeCol =  GetColumn(j, matrix)
            var upTrees = treeCol.slice(0,i)
            var downTrees = treeCol.slice(i+1, treeLine.size)
            isVisible = upTrees.forall(_ < el) || downTrees.forall(_ < el)
        }    
        isVisible
    }

    def part1(lines: List[String]): Int = {
        var inputMatrix = Array.ofDim[Int](ROW_NUM, COL_NUM)
        var treesCount = 0
        
        var idx = 0
        var arr: Array[Int] = Array()
        for(l <- lines){
            arr = Array()
            l.toCharArray().toList.foreach(i => arr = arr :+ i.asDigit)
            inputMatrix(idx) = arr
            idx += 1
        }

        for(i <- 0 to ROW_NUM-1){
            for (j <- 0 to COL_NUM-1){
                var el = inputMatrix(i)(j)
                if (IsVisible(el, i, j, inputMatrix)){
                    //println(el + "(" + i + "," + j + ")")
                    treesCount += 1
                }
            }
        }

        treesCount
    }

    def CalculateScore(el: Int, i: Int, j: Int, matrix: Array[Array[Int]]): Int = {
        var countTreesUp, countTreesDown, countTreesLeft, countTreesRight = 0

        if (i == 0 || j == 0 || i == ROW_NUM-1 || j == COL_NUM-1){
            return 0
        } 

        var treeLine =  matrix(i)
        var leftTrees = treeLine.slice(0,j)
        var rightTrees = treeLine.slice(j+1, treeLine.size)
        var blockingTreeIndex = leftTrees.reverse.indexWhere( _ >= el)
        if (blockingTreeIndex == -1){
            countTreesLeft = j 
        } else {
            countTreesLeft = j - (leftTrees.size - blockingTreeIndex) + 1  
        }
        
        blockingTreeIndex = rightTrees.indexWhere(_ >= el)
        if (blockingTreeIndex == -1){
            countTreesRight = COL_NUM - (j+1)
        } else {
            countTreesRight = blockingTreeIndex + 1 
        }
        
        var treeCol =  GetColumn(j, matrix)
        var upTrees = treeCol.slice(0,i)
        var downTrees = treeCol.slice(i+1, treeLine.size)

        blockingTreeIndex = upTrees.reverse.indexWhere( _ >= el)
        if (blockingTreeIndex == -1){
            countTreesUp = i
        } else {
            countTreesUp = i - (upTrees.size - blockingTreeIndex) + 1 
        }

        blockingTreeIndex = downTrees.indexWhere(_ >= el)
        if (blockingTreeIndex == -1){
            countTreesDown = COL_NUM - (i+1)
        } else {
            countTreesDown = blockingTreeIndex + 1 
        }

        countTreesLeft * countTreesRight * countTreesUp * countTreesDown
    }

    def part2(lines: List[String]): Int = {
        var inputMatrix = Array.ofDim[Int](ROW_NUM, COL_NUM)
        var highestScore = -1
        
        var idx = 0
        var arr: Array[Int] = Array()
        for(l <- lines){
            arr = Array()
            l.toCharArray().toList.foreach(i => arr = arr :+ i.asDigit)
            inputMatrix(idx) = arr
            idx += 1
        }

        for(i <- 0 to ROW_NUM-1){
            for (j <- 0 to COL_NUM-1){
                var el = inputMatrix(i)(j)
                var score = CalculateScore(el, i, j, inputMatrix)
                if(score > highestScore){
                    highestScore = score
                }
            }
        }
        highestScore
    }
}