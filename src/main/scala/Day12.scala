import scala.collection.mutable.ListBuffer
import scala.collection.immutable.BitSet

class Day12 {
    var ROW_NUM = 41
    var COL_NUM = 143
    var alphabet = "abcdefghijklmnopqrstuvwxyz"
    
    def CalculateDist(a: Char, b:Char, currentCount: Int): Int = {
        var bb = b
        var aa = a
        if (b == 'E'){
            bb = 'z'
        }
        if (a == 'E'){
            aa = 'z'
        }

        if (a == 'S'){
            aa = 'a'
        }
        if (b == 'S'){
            bb = 'a'
        }

        if(alphabet.indexOf(bb) - alphabet.indexOf(aa) > 1){
          return Int.MaxValue
        }

        if(alphabet.indexOf(bb) - alphabet.indexOf(aa) <= 1){ 
          return 0 + 1 + currentCount
        } 
        
        if(alphabet.indexOf(aa) > alphabet.indexOf(bb)){
          return 0 + 1 + currentCount
        }
        
        return Int.MaxValue
    }

    def minDistance(source: (Int, Int), target: (Int,Int), matrix: Array[Array[Char]]): Int = {
        var queue: Set[(Int, Int, Int)] = Set()
        queue += ((source._1, source._2, 0))
 
    var calculatedVisits: Array[Array[Int]] =  Array.tabulate(ROW_NUM,COL_NUM)( (x,y) => Int.MaxValue )
    calculatedVisits(source._1)(source._2) = 0
    var minCost = Int.MaxValue
 
    while (!queue.isEmpty) {
      var p = queue.head
      queue = queue.tail

      // Target found
      if (matrix(p._1)(p._2) == 'E'){
        if (p._3 < minCost){
          minCost = p._3
        }
      }
 
      // moving up
      if (isValid(p._1 - 1, p._2, matrix)) {
        var thisDistance = CalculateDist(matrix(p._1)(p._2), matrix(p._1 - 1)(p._2), p._3)

        var currentDist = calculatedVisits(p._1 - 1)(p._2) 
        if((currentDist == Int.MaxValue && thisDistance < Int.MaxValue) || (currentDist < Int.MaxValue && currentDist >= thisDistance)){
            calculatedVisits(p._1 - 1)(p._2) = thisDistance
            queue += ((p._1 - 1, p._2, thisDistance))
        }
      }
 
      // moving down
      if (isValid(p._1 + 1, p._2, matrix)) {
        var thisDistance = CalculateDist(matrix(p._1)(p._2), matrix(p._1 + 1)(p._2), p._3)

        var currentDist = calculatedVisits(p._1 + 1)(p._2) 
        if((currentDist == Int.MaxValue && thisDistance < Int.MaxValue) || (currentDist < Int.MaxValue && currentDist >= thisDistance)){
            calculatedVisits(p._1 + 1)(p._2) = thisDistance
            queue += ((p._1 + 1, p._2, thisDistance))
        }
      }
 
      // moving left
      if (isValid(p._1, p._2 - 1, matrix)) {
        var thisDistance = CalculateDist(matrix(p._1)(p._2), matrix(p._1)(p._2 - 1), p._3)

        var currentDist = calculatedVisits(p._1)(p._2 - 1) 
        if((currentDist == Int.MaxValue && thisDistance < Int.MaxValue) || (currentDist < Int.MaxValue && currentDist >= thisDistance)){
            calculatedVisits(p._1)(p._2 - 1) = thisDistance
            queue += ((p._1, p._2 - 1, thisDistance))
        }
      }
 
      // moving right
      if (isValid(p._1, p._2 + 1, matrix)) {
        var thisDistance = CalculateDist(matrix(p._1)(p._2), matrix(p._1)(p._2 + 1), p._3)

        var currentDist = calculatedVisits(p._1)(p._2 + 1) 
        if((currentDist == Int.MaxValue && thisDistance < Int.MaxValue) || (currentDist < Int.MaxValue && currentDist >= thisDistance)){
            calculatedVisits(p._1)(p._2 + 1) = thisDistance
            queue += ((p._1, p._2 + 1, thisDistance))
        }
      }
    }
    return minCost
  }
   
  def isValid(x: Int, y: Int, matrix: Array[Array[Char]]): Boolean =
  {
    if (x >= 0 && y >= 0 && x < matrix.length
        && y < matrix(0).length) {
      return true
    }
    return false
  }
 

  def part1(lines: List[String]): Int = {
      var map: Array[Array[Char]] = Array.ofDim[Char](ROW_NUM, COL_NUM)
      var minSteps = 0
      var idx = 0
      var arr: Array[Char] = Array()
      var startCoord = (0,0)
      var endCoord = (0,0)
      for(l <- lines){
        arr = Array()
        l.toCharArray().toList.foreach(i => arr = arr :+ i.toChar)
        map(idx) = arr
            
        if(l.contains("S")){
          startCoord = (idx, l.indexOf("S"))
        }
            
        if(l.contains("E")){
          endCoord = (idx, l.indexOf("E"))
        }
          idx += 1
        }

      minSteps = minDistance(startCoord, endCoord, map)
      minSteps
    }

  def part2(lines: List[String]): Int = {
    var map: Array[Array[Char]] = Array.ofDim[Char](ROW_NUM, COL_NUM)
    var minSteps = 0
    var idx = 0
    var arr: Array[Char] = Array()
    var endCoord = (0,0)
    var startOptions: Array[(Int,Int)] = Array()

    for(l <- lines){
      arr = Array()
      l.toCharArray().toList.foreach(i => arr = arr :+ i.toChar)
      map(idx) = arr
            
      if(l.contains("S")){
        startOptions = startOptions :+ (idx, l.indexOf("S"))
      }

      if(l.contains("a")){
        var allLineChars = l.split("")
        var i = 0
        for(c <- allLineChars){
          if(c == "a"){
            startOptions = startOptions :+ (idx, i)
          }
          i += 1
        }
      }
            
      if(l.contains("E")){
        endCoord = (idx, l.indexOf("E"))
      }
      idx += 1
    }

    var minPathSteps = Int.MaxValue
    for(start <- startOptions){
      var pathSteps = minDistance(start, endCoord, map)
      if(pathSteps < minPathSteps){
        minPathSteps = pathSteps
      }
    }

    minPathSteps  
  }
}