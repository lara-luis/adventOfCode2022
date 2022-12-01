class Day1 {
    def part1(lines: List[String]): Int = {
        var maxCalories = -1
        var currentElfCaloriesList : List[Int] = List.empty;

        for(l <- lines){
            if(l.nonEmpty){
                currentElfCaloriesList = currentElfCaloriesList :+ l.toInt
            } else {
                var currentElfCaloriesTotal = currentElfCaloriesList.sum
                if (currentElfCaloriesTotal > maxCalories){
                    maxCalories = currentElfCaloriesTotal
                } 
                currentElfCaloriesTotal = -1
                currentElfCaloriesList = Nil
            }
        } 
        maxCalories
    }

    def part2(lines: List[String]): Int = {
        var sumOfCaloriesPerElfList : List[Int] = List.empty;
        var currentElfCaloriesList : List[Int] = List.empty;

        for(l <- lines){
            if(l.nonEmpty){
                currentElfCaloriesList = currentElfCaloriesList :+ l.toInt
            } else {
                var currentElfCaloriesTotal = currentElfCaloriesList.sum
                sumOfCaloriesPerElfList = sumOfCaloriesPerElfList :+ currentElfCaloriesTotal
                currentElfCaloriesList = Nil
            }
        } 

        var orderedCalories = sumOfCaloriesPerElfList.sorted(Ordering.Int.reverse)
        var relevantElfsCalories = orderedCalories take 3
        relevantElfsCalories.sum
    }
}