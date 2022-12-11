class Day11 {
    var MONKEY_NUM = 8

    def part1(lines: List[String]): Int = {
        var monkeyItems: Array[(Int /*number of inspected items*/, Array[String] /*items per monkey where index  represents the monkey*/)] = Array.ofDim[(Int,Array[String])](MONKEY_NUM)
        var monkeyData: Array[(String /*operation*/, String /*test*/, String /*true*/, String /*false*/)] = Array.ofDim[(String,String,String,String)](MONKEY_NUM)
        var monkeyBusiness = 0
        var currentMonkey = -1
        var currentOp = ""
        var currentTest = ""
        var currentTrueBranch = ""
        var currentFalseBranch = ""

        for(l <- lines){
            if(l.startsWith("Monkey ")){
                currentMonkey = l.replace("Monkey ", "").replace(":", "").toInt
            }
            if(l.trim().startsWith("Starting items")){
                var itemString = l.replace("Starting items: ", "")
                var items = itemString.split(",")
                monkeyItems(currentMonkey) = (0, items)
            }
            if(l.trim().startsWith("Operation:")){
                var op = l.replace(" Operation: new =", "")
                currentOp = op
            }
            if(l.trim().startsWith("Test: divisible by")){
                var test = l.replace("Test: divisible by", "")
                currentTest = test
            }
            if(l.trim().startsWith("If true")){
                var trueBranch = l.replace("If true: throw to monkey ", "")
                currentTrueBranch = trueBranch
            }
            if(l.trim().startsWith("If false")){
                var falseBranch = l.replace("If false: throw to monkey ", "")
                currentFalseBranch = falseBranch

                monkeyData(currentMonkey) = (currentOp, currentTest, currentTrueBranch, currentFalseBranch)
            }
        }
    
        // 20 rounds
        var cycleMonkeyItems = monkeyItems
        for(i <- 1 to 20){
            for(monkeyNum <- 0 to MONKEY_NUM-1){
                var monkey = cycleMonkeyItems(monkeyNum)

                if(monkey == null){
                    // monkey doesn't have anything to process now
                } else {
                    var currentMonkeyEvaluations = monkey._1
                    var currentMonkeyItems = monkey._2
                    var currentMonkeyData = monkeyData(monkeyNum)
                    for(item <- currentMonkeyItems){
                        var itemWorryLevel = item.trim().toInt
                        var op = currentMonkeyData._1
                        var newWorryLevel = -1
                        if(op.contains("old *")){
                            var n = op.replace("old *", "")
                            if (n.trim() == "old"){
                                newWorryLevel = itemWorryLevel * itemWorryLevel
                            } else {
                                newWorryLevel = itemWorryLevel * n.trim().toInt
                            }
                        } 
                        if(op.contains("old +")){
                        var n = op.replace("old +", "")
                            if (n.trim() == "old"){
                                newWorryLevel = itemWorryLevel + itemWorryLevel
                            } else {
                                newWorryLevel = itemWorryLevel + n.trim().toInt
                            }
                        }
                        var wl = newWorryLevel.toDouble / 3
                        var worryLevelAfterBoredom = math.floor(wl)
                        var worryLevelAfterBoredomTestResult = worryLevelAfterBoredom % currentMonkeyData._2.trim().toInt == 0
                        var newAssignedMonkey = -1
                        if(worryLevelAfterBoredomTestResult){
                            newAssignedMonkey = currentMonkeyData._3.trim().toInt
                        } else {
                            newAssignedMonkey = currentMonkeyData._4.trim().toInt
                        }

                        cycleMonkeyItems(monkeyNum) = (currentMonkeyEvaluations + currentMonkeyItems.length, Array.empty)
                        var assignedMonkeyData = cycleMonkeyItems(newAssignedMonkey)
                        if(assignedMonkeyData == null){
                            assignedMonkeyData = (0, Array.empty)
                        } 
                        cycleMonkeyItems(newAssignedMonkey) = (assignedMonkeyData._1, assignedMonkeyData._2 :+ worryLevelAfterBoredom.toInt.toString())
                        //clean current monkey items
                        // add new items to other monkey item list
                    }
                }
            }
        }       

        var monkeysScores: Array[Int] = Array.empty
        for (m <- cycleMonkeyItems){
            monkeysScores = monkeysScores :+ m._1
        } 

        monkeysScores = monkeysScores.sorted(Ordering.Int.reverse)
        monkeyBusiness = monkeysScores(0) * monkeysScores(1)
        monkeyBusiness
    }

    def part2(lines: List[String]): Long = {
                var monkeyItems: Array[(BigInt /*number of inspected items*/, Array[String] /*items per monkey where index  represents the monkey*/)] = Array.ofDim[(BigInt,Array[String])](MONKEY_NUM)
        var monkeyData: Array[(String /*operation*/, String /*test*/, String /*true*/, String /*false*/)] = Array.ofDim[(String,String,String,String)](MONKEY_NUM)
        var monkeyBusiness:Double = 0
        var currentMonkey = -1
        var currentOp = ""
        var currentTest = ""
        var currentTrueBranch = ""
        var currentFalseBranch = ""

        for(l <- lines){
            if(l.startsWith("Monkey ")){
                currentMonkey = l.replace("Monkey ", "").replace(":", "").toInt
            }
            if(l.trim().startsWith("Starting items")){
                var itemString = l.replace("Starting items: ", "")
                var items = itemString.split(",")
                monkeyItems(currentMonkey) = (0, items)
            }
            if(l.trim().startsWith("Operation:")){
                var op = l.replace(" Operation: new =", "")
                currentOp = op
            }
            if(l.trim().startsWith("Test: divisible by")){
                var test = l.replace("Test: divisible by", "")
                currentTest = test
            }
            if(l.trim().startsWith("If true")){
                var trueBranch = l.replace("If true: throw to monkey ", "")
                currentTrueBranch = trueBranch
            }
            if(l.trim().startsWith("If false")){
                var falseBranch = l.replace("If false: throw to monkey ", "")
                currentFalseBranch = falseBranch

                monkeyData(currentMonkey) = (currentOp, currentTest, currentTrueBranch, currentFalseBranch)
            }
        }
    
        // 10000 rounds
        var cycleMonkeyItems = monkeyItems
        for(i <- 1 to 10000){
            //println("cycle " + i)
            for(monkeyNum <- 0 to MONKEY_NUM-1){
                var monkey = cycleMonkeyItems(monkeyNum)

                if(monkey == null){
                    // monkey doesn't have anything to process now
                } else {
                    var currentMonkeyEvaluations:BigInt = monkey._1
                    var currentMonkeyItems = monkey._2
                    var currentMonkeyData = monkeyData(monkeyNum)
                    for(item <- currentMonkeyItems){
                        var itemWorryLevel = item.trim().toDouble
                        var op = currentMonkeyData._1
                        var newWorryLevel:Double = 0
                        if(op.contains("old *")){
                            var n = op.replace("old *", "")
                            if (n.trim() == "old"){
                                newWorryLevel = itemWorryLevel * itemWorryLevel
                            } else {
                                newWorryLevel = itemWorryLevel * n.trim().toDouble
                            }
                        } 
                        if(op.contains("old +")){
                        var n = op.replace("old +", "")
                            if (n.trim() == "old"){
                                newWorryLevel = itemWorryLevel + itemWorryLevel
                            } else {
                                newWorryLevel = itemWorryLevel + n.trim().toDouble
                            }
                        }

                        //9699690 - the random number that results from multiply every input diviser
                        newWorryLevel = newWorryLevel % 9699690
                        var worryLevelAfterBoredomTestResult = newWorryLevel % currentMonkeyData._2.trim().toInt == 0
                        var newAssignedMonkey = -1
                        if(worryLevelAfterBoredomTestResult){
                            newAssignedMonkey = currentMonkeyData._3.trim().toInt
                        } else {
                            newAssignedMonkey = currentMonkeyData._4.trim().toInt
                        }

                        cycleMonkeyItems(monkeyNum) = (currentMonkeyEvaluations + currentMonkeyItems.length, Array.empty)
                        var assignedMonkeyData = cycleMonkeyItems(newAssignedMonkey)
                        if(assignedMonkeyData == null){
                            assignedMonkeyData = (0, Array.empty)
                        } 
                        cycleMonkeyItems(newAssignedMonkey) = (assignedMonkeyData._1, assignedMonkeyData._2 :+ newWorryLevel.toString())
                    }
                }
            }

            if(i == 1 || i == 20 || i == 1000 || i == 2000 || i == 3000 || i == 4000 || 
            i == 5000 || i == 6000 || i == 7000 || i == 8000 || i == 9000 || i == 10000){
                var monkeysScores: Array[Int] = Array.empty
                var output = ""
                for (m <- cycleMonkeyItems){
                    output = output + " " + m._1.toString()
                }
                println(output)
            } 
        }       

        var monkeysScores: Array[Double] = Array.empty
        for (m <- cycleMonkeyItems){
            monkeysScores = monkeysScores :+ m._1.toDouble
        } 

        monkeysScores = monkeysScores.sorted(Ordering[Double].reverse)
        monkeyBusiness = monkeysScores(0) * monkeysScores(1)
        monkeyBusiness.toLong
    }
}