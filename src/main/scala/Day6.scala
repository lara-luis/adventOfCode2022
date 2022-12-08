class Day6 {
    def part1(lines: List[String]): Int = {
        var countChars = 0
        var marker = ""
       
        for(line <- lines){
            var chars = line.toCharArray()
            var length = chars.length - 4
        
            var i = 0
            while(i < length && countChars == 0){
                var tmpArray = List ( chars(i), chars(i+1), chars(i+2), chars(i+3) )
                if (tmpArray.distinct.length == 4){
                    countChars = (i+3)+1
                }
                i += 1  
            }
        }
        countChars
    }

    def part2(lines: List[String]): Int = {
     var countChars = 0
        var marker = ""
       
        for(line <- lines){
            var chars = line.toCharArray()
            var length = chars.length - 14
        
            var i = 0
            while(i < length && countChars == 0){
                var tmpArray = List ( chars(i), chars(i+1), chars(i+2), chars(i+3), chars(i+4), chars(i+5), chars(i+6), chars(i+7), chars(i+8), chars(i+9), chars(i+10), chars(i+11), chars(i+12), chars(i+13) )
                if (tmpArray.distinct.length == 14){
                    countChars = (i+13)+1
                }
                i += 1  
            }
        }
        countChars
    }
}