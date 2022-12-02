class Day2 {
    private def GetMyRoundPoints_1(oponentChoice: String, myChoise: String): Int = {
        var points = 0;
        var losePoints = 0;
        var drawnPoints = 3;
        var winPoints = 6;

        var A, X = 1
        var B, Y = 2
        var C, Z = 3

        oponentChoice match {
            case "A" => if (myChoise == "X") points = drawnPoints + X
                        else if(myChoise == "Y") points = winPoints + Y
                        else points = losePoints + Z
            case "B" => if (myChoise == "X") points = losePoints + X
                        else if(myChoise == "Y") points = drawnPoints + Y
                        else points = winPoints + Z
            case "C" => if (myChoise == "X") points = winPoints + X
                        else if(myChoise == "Y") points = losePoints + Y
                        else points = drawnPoints + Z
        }
        
        points
    }

    private def GetMyRoundPoints_2(oponentChoice: String, myChoise: String): Int = {
        var points = 0;
        var losePoints = 0;
        var drawnPoints = 3;
        var winPoints = 6;

        var A, X = 1
        var B, Y = 2
        var C, Z = 3

        oponentChoice match {
            case "A" => if (myChoise == "X") points = losePoints + C
                        else if(myChoise == "Y") points = drawnPoints + A
                        else points = winPoints + B
            case "B" => if (myChoise == "X") points = losePoints + A
                        else if(myChoise == "Y") points = drawnPoints + B
                        else points = winPoints + C
            case "C" => if (myChoise == "X") points = losePoints + B
                        else if(myChoise == "Y") points = drawnPoints + C
                        else points = winPoints + A
        }
        
        points
    }

    def part1(lines: List[String]): Int = {
        var totalPoints = 0
        for(l <- lines){
            var game = l.split(" ")
            var roundPoints = GetMyRoundPoints_1(game(0), game(1))
            totalPoints = totalPoints + roundPoints
        }
        totalPoints
    }

    def part2(lines: List[String]): Int = {
        var totalPoints = 0
        for(l <- lines){
            var game = l.split(" ")
            var roundPoints = GetMyRoundPoints_2(game(0), game(1))
            totalPoints = totalPoints + roundPoints
        }
        totalPoints
    }
}