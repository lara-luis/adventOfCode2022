import scala.collection.mutable.ListBuffer
import scala.collection.immutable.BitSet

class Day15 {
  def TARGET_Y = 10

  def GetForbiddenBeaconZones(lines: List[String]): SortedSet[Int] = {
    var sensorsDistances: List[(Int /*X*/, Int /*Y*/, Int /*distance*/)] = List.empty
    var filledPosInLineY: Set[Int] = Set.empty
    var beacons: List[(Int,Int)] = List.empty 

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
      beacons = beacons :+ (beaconX, beaconY)
    }

    var notBeaconCount = 0
    for(s <- sensorsDistances){
      var rangeLeft = -1
      var rangeRight = -1
      //number of spots affected by the sensor
      if(s._2 >= TARGET_Y) {
        var currentY = s._2 - s._3 //starting max y it will cover
        var rangeMinY = s._1
        var rangeMaxY = s._1

        if(currentY == TARGET_Y){
          filledPosInLineY += s._1
        } else if(currentY < TARGET_Y){
          filledPosInLineY += s._1
          while(currentY < TARGET_Y){
            rangeMinY -= 1
            rangeMaxY += 1
            filledPosInLineY += rangeMinY
            filledPosInLineY += rangeMaxY
            currentY += 1
          }
        }
      } else if (s._2 <= TARGET_Y) {
        var currentY = s._2 + s._3 //starting max y it will cover
        var rangeMinY = s._1
        var rangeMaxY = s._1

        if(currentY == TARGET_Y){
          filledPosInLineY += s._1
        } else if(currentY > TARGET_Y){
          filledPosInLineY += s._1
          while(currentY > TARGET_Y){
            rangeMinY -= 1
            rangeMaxY += 1
            filledPosInLineY += rangeMinY
            filledPosInLineY += rangeMaxY
            currentY -= 1
          }
        }
      }
    }

    for(xCoord <- filledPosInLineY){
      var existsBeaconInSamePos = beacons.exists( c => c._1 == xCoord && c._2 == TARGET_Y)
      if(existsBeaconInSamePos){
        filledPosInLineY -= (xCoord)
      }
    }
    filledPosInLineY
  }


  def part1(lines: List[String]): Int = {
    var lineForbiddenX = GetForbiddenBeaconZones(lines)    

    lineForbiddenX.size
  }
  
  def part2(lines: List[String]): Int = {
    var max_x = 20
    var max_y = 20
    var result_x = -1
    var result_y = -1
    var frequency = -1

    for(test_y <- 0 to max_y){
      var potential_xs = GetForbiddenBeaconZones(lines)
      potential_xs.filter(x => x <=20)
      if(potential_xs.nonEmpty){
        result_y = test_y
        result_x = 
      }
    }

    result_x*4000000 + result_y
  }
}