class Day4 {
    def part1(lines: List[String]): Int = {
        var containedPairsCount = 0
        for(l <- lines){
            val pairs = l.split(",")
            val pair1 = pairs(0).split("-")
            val pair2 = pairs(1).split("-")
            val pair1Range = Set.range(pair1(0).toInt, pair1(1).toInt+1)
            val pair2Range = Set.range(pair2(0).toInt, pair2(1).toInt+1)
            if (pair1Range.subsetOf(pair2Range) || pair2Range.subsetOf(pair1Range)) {
                containedPairsCount += 1
            }
        }
        containedPairsCount
    }

    def part2(lines: List[String]): Int = {
        var containedPairsCount = 0
        for(l <- lines){
            val pairs = l.split(",")
            val pair1 = pairs(0).split("-")
            val pair2 = pairs(1).split("-")
            val pair1Range = Set.range(pair1(0).toInt, pair1(1).toInt+1)
            val pair2Range = Set.range(pair2(0).toInt, pair2(1).toInt+1)
            val mergedRanges = pair1Range ++ pair2Range
            if (mergedRanges.size < (pair1Range.size + pair2Range.size)) {
                containedPairsCount += 1
            }
        }
        containedPairsCount
    }
}