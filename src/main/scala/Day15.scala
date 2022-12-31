import scala.collection.mutable.ListBuffer
import scala.collection.immutable.BitSet
import scala.util.control.Breaks._
import scala.collection.immutable.SortedSet
import scala.math.Numeric

class Day15 {
  def ProcessLines(lines: List[String]): (List[(Int /*X*/, Int /*Y*/, Int /*distance*/)], Set[(Int,Int)]) = {
    var sensorsDistances: List[(Int /*X*/, Int /*Y*/, Int /*distance*/)] = List.empty
    var beacons: Set[(Int,Int)] = Set.empty 

    for(l <- lines){
      var ll = l.replace("Sensor at ","")
      var coords = ll.split(": closest beacon is at ")  
      var sensor = coords(0).split(", ")
      var sensorX = sensor(0).replace("x=","").toInt
      var sensorY = sensor(1).replace("y=","").toInt
      var beacon = coords(1).split(", ")
      var beaconX = beacon(0).replace("x=","").toInt
      var beaconY = beacon(1).replace("y=","").toInt
      var manhattanDistance = (sensorX - beaconX).abs + (sensorY - beaconY).abs
      sensorsDistances = sensorsDistances :+ (sensorX, sensorY, manhattanDistance.toInt)
      beacons += ((beaconX, beaconY))
    }
    (sensorsDistances, beacons)
  } 

  def CalculateInterval(intervals: Set[(Int,Int)], newRangeMin: Int, newRangeMax: Int): Set[(Int,Int)] = {
    var iterator = intervals.toList.iterator
    var index = 0
    var res: Set[(Int,Int)] = Set.empty
    var matched: Boolean = false
    var maxRangeSize = 0

    if(intervals.isEmpty){
      res += ((newRangeMin, newRangeMax))
    } else {
      var pendingToAdd: Set[(Int,Int)] = Set.empty
      while(iterator.hasNext){
        var interval = iterator.next()
        var matchedIt = false

        if (interval._1 >= newRangeMin && interval._2 <= newRangeMax){
          res += ((newRangeMin, newRangeMax))
          matched = true
          matchedIt = true
        } else if (interval._1 >= newRangeMin && interval._2 >= newRangeMax 
                  && interval._1 <= newRangeMax){
          res += ((newRangeMin, interval._2))
          matched = true
          matchedIt = true
        } else if (interval._1 <= newRangeMin && interval._2 <= newRangeMax
                 && interval._1 >= newRangeMin){
          res += ((interval._1, newRangeMax))
          matched = true
          matchedIt = true
        } else if(interval._1 <= newRangeMin && interval._2 >= newRangeMax){
          var currRange = interval._2 - interval._1
          if(currRange > maxRangeSize){
            res += ((interval._1, interval._2))
            matched = true
            matchedIt = true
            maxRangeSize = currRange
          }
        } else if(interval._1 <= newRangeMax && interval._1 >= newRangeMin){
          res += ((newRangeMin, interval._2))
          matched = true
          matchedIt = true
        } else if(interval._2 >= newRangeMin && interval._2 <= newRangeMax){
          res += ((interval._1, newRangeMax))
          matched = true
          matchedIt = true
        }

        index += 1

        if(!matchedIt){
          pendingToAdd += ((interval._1, interval._2))
        }
      }

      if(!matched){
        res += ((newRangeMin, newRangeMax))
      }
      res ++= pendingToAdd
    }
    res
  }

  def GetForbiddenBeaconZones_part2(lines: List[String], targetY: Int, 
    sensorsDistances: List[(Int /*X*/, Int /*Y*/, Int /*distance*/)],
    beacons: Set[(Int,Int)], part: Int): (Int, Int) = {
    var intervals: Set[(Int,Int)] = Set.empty
    var minXOverall = Int.MaxValue
    var maxXOverall = Int.MinValue

    var iterator = sensorsDistances.iterator
    while(iterator.hasNext){
      var s = iterator.next()

      var rangeLeft = -1
      var rangeRight = -1
      //number of spots affected by the sensor
      if(s._2 >= targetY) {
        var currentY = s._2 - s._3 //starting max y it will cover
        var rangeMinX = s._1
        var rangeMaxX = s._1

        if(currentY == targetY){
          intervals = CalculateInterval(intervals, s._1, s._1)
          
          if(s._1 > maxXOverall){
            maxXOverall = s._1
          }
          if(s._1 < minXOverall){
            minXOverall = s._1
          }
        } else if(currentY < targetY){
          intervals = CalculateInterval(intervals, s._1, s._1)
          var yDiff = targetY - currentY
          rangeMinX -= yDiff
          rangeMaxX += yDiff

          intervals = CalculateInterval(intervals, rangeMinX, rangeMaxX)
          
          if(rangeMaxX > maxXOverall){
            maxXOverall = rangeMaxX
          }
          if(rangeMinX < minXOverall){
            minXOverall = rangeMinX
          }
        }
      } else if (s._2 <= targetY) {
        var currentY = s._2 + s._3 //starting the max y it will cover
        var rangeMinX = s._1
        var rangeMaxX = s._1

        if(currentY == targetY){
          intervals = CalculateInterval(intervals, s._1, s._1)
          
          if(s._1 > maxXOverall){
            maxXOverall = s._1
          }
          if(s._1 < minXOverall){
            minXOverall = s._1
          }
        } else if(currentY > targetY){
          intervals = CalculateInterval(intervals, s._1, s._1)
          var yDiff = currentY - targetY
          rangeMinX -= yDiff
          rangeMaxX += yDiff

          var range = (rangeMinX to rangeMaxX)
          intervals = CalculateInterval(intervals, rangeMinX, rangeMaxX)
          
          if(rangeMaxX > maxXOverall){
            maxXOverall = rangeMaxX
          }
          if(rangeMinX < minXOverall){
            minXOverall = rangeMinX
          }
        }
      }
    }

    var processedRanges: Set[(Int, Int)] = intervals
    var i = 0
    var it = processedRanges.toList.iterator
    while(it.hasNext && processedRanges.size > 1){
      var interval = it.next()
      var r = CalculateInterval(processedRanges, interval._1, interval._2)
      if(r != processedRanges){
        processedRanges = r
        it = processedRanges.toList.iterator
      }
      i += 1
    }

    var resultX = -1
    if(processedRanges.size > 1){
      resultX = processedRanges.head._2 + 1 // the missing number is guaranteed to be only one 
      // so we can find it just adding +1 after the last digit of the first covered range
    }

    (resultX, targetY)
  }


  def GetForbiddenBeaconZones_part1(lines: List[String], targetY: Int, 
  sensorsDistances: List[(Int /*X*/, Int /*Y*/, Int /*distance*/)],
  beacons: Set[(Int,Int)], part: Int): (SortedSet[Int], Boolean) /*no beacons Xs*/= {
    var filledPosInLineY: SortedSet[Int] = SortedSet.empty
    var notBeaconCount = 0
    var lastRowElement = -1
    for(s <- sensorsDistances){
      var rangeLeft = -1
      var rangeRight = -1
      //number of spots affected by the sensor
      if(s._2 >= targetY) {
        var currentY = s._2 - s._3 //starting max y it will cover
        var rangeMinX = s._1
        var rangeMaxX = s._1

        if(currentY == targetY){
          filledPosInLineY += s._1
          lastRowElement = s._1
        } else if(currentY < targetY){
          filledPosInLineY += s._1
          var yDiff = targetY - currentY
          rangeMinX -= yDiff
          rangeMaxX += yDiff
          filledPosInLineY ++= (rangeMinX to rangeMaxX)
          lastRowElement = rangeMaxX
        }
      } else if (s._2 <= targetY) {
        var currentY = s._2 + s._3 //starting max y it will cover
        var rangeMinX = s._1
        var rangeMaxX = s._1

        if(currentY == targetY){
          filledPosInLineY += s._1
          lastRowElement = s._1
        } else if(currentY > targetY){
          filledPosInLineY += s._1
          var yDiff = currentY - targetY
          rangeMinX -= yDiff
          rangeMaxX += yDiff
          filledPosInLineY ++= (rangeMinX to rangeMaxX)
          lastRowElement = rangeMaxX
        }
      }
    }

    if(part == 1){
      for(xCoord <- filledPosInLineY){
        var existsBeaconInSamePos = beacons.contains((xCoord,targetY))
        if(existsBeaconInSamePos){
          filledPosInLineY -= (xCoord)
        }
      }
    }

    var resFirstInt = filledPosInLineY.head
    var resLastInt = lastRowElement
    var totalNumOfElementsExpected = (resLastInt - resFirstInt) + 1
    (filledPosInLineY, totalNumOfElementsExpected == filledPosInLineY.size)
  }


  def part1(lines: List[String]): Int = {
    var targetY = 2000000
    var dataFromInput = ProcessLines(lines)
    var sensorsDistances = dataFromInput._1
    var beacons = dataFromInput._2

    val (lineForbiddenX, _) = GetForbiddenBeaconZones_part1(lines, targetY, sensorsDistances, beacons, 1)    
    lineForbiddenX.size
  }
  
  def part2(lines: List[String]): BigInt = {
    var max_x = 4000000
    var max_y = 4000000
    var result_x = -1
    var result_y = -1
    var frequency = -1
    var dataFromInput = ProcessLines(lines)
    var sensorsDistances = dataFromInput._1
    var beacons = dataFromInput._2

    var test_y = 0
    while(test_y <= max_y){
      var t1 = System.currentTimeMillis()
      var (result_x, result_y) = GetForbiddenBeaconZones_part2(lines, test_y, sensorsDistances, beacons, 2)
      var duration = System.currentTimeMillis() - t1

      if (result_x != -1){
        var result: BigInt = (result_x*4000000 + result_y)
        println(result)
        return result
      }
      
      test_y += 1
    }
    0
  }
}