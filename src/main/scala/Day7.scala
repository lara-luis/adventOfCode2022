class Day7 {
    var cache: Map[String, Int] = Map.empty

    def GetDirSize(dir: String, parentWithChildrenMap: Map[String, List[String]], sizesMap: Map[String, String]) : Int = {
        var cachedSize = cache.get(dir)
        if (cachedSize.isDefined){
            return cachedSize.get
        } 

        var children = parentWithChildrenMap(dir)
        var sum = 0
        for (c <- children) {
            var size = sizesMap.getOrElse(c, null) // if it is in this map, it is a file else it is a file
            if(size != null){
                sum += size.toInt
            } else {
                sum += GetDirSize(c, parentWithChildrenMap, sizesMap)
            }
        }

        cache = cache + (dir -> sum)
        sum
    }
    
    def part1(lines: List[String]): Int = {
        var sumOfSizes = 0
        var sizesMap: Map[String, String] = Map.empty[String, String]
        var parentWithChildrenMap: Map[String, List[String]] = Map().withDefaultValue(List.empty[String])
        var currentDir = ""
        var allDirs: Set[String] = Set.empty
        var childParents: Map[String, String] = Map.empty[String, String]

        for(line <- lines){
            if (line.startsWith("$ cd ..")){
                var parent = childParents(currentDir)
                currentDir = parent
            } else if (line.startsWith("$ cd")){
                var oldCurrentDir = currentDir
                currentDir = oldCurrentDir + "/" + line.substring(4).trim()
                allDirs += currentDir
            } else if (line.startsWith("$ ls")){
                // stuff
            } else if (line.startsWith("dir")){ 
                var dirName = line.substring(4).trim()
                var childOfCurrentDir = parentWithChildrenMap.getOrElse(currentDir, List.empty)
                childOfCurrentDir = childOfCurrentDir :+ (currentDir + "/" + dirName)
                parentWithChildrenMap = parentWithChildrenMap.updated(currentDir, childOfCurrentDir)
                allDirs += currentDir
                childParents = childParents + (currentDir + "/" + dirName -> currentDir)
            } else {                
                // starts with file size

                var fileInfo = line.split(" ")
                var fileSize = fileInfo(0)
                var fileName = fileInfo(1)
                sizesMap = sizesMap + (currentDir + "/" + fileName -> fileSize) 
                var parentList = parentWithChildrenMap.getOrElse(currentDir, List.empty)
                parentList = parentList :+ currentDir + "/" + fileName
                parentWithChildrenMap = parentWithChildrenMap.updated(currentDir, parentList)
                childParents = childParents + (currentDir + "/" + fileName -> currentDir)
            }
        }
        
        for(d <- allDirs){
            var sum = GetDirSize(d, parentWithChildrenMap, sizesMap)
            if (sum < 100000) {
                sumOfSizes += sum
            }
        }
        sumOfSizes
    }

    def part2(lines: List[String]): Int = {
        var sumOfSizes = 0
        var sizesMap: Map[String, String] = Map.empty[String, String]
        var parentWithChildrenMap: Map[String, List[String]] = Map().withDefaultValue(List.empty[String])
        var currentDir = ""
        var allDirs: Set[String] = Set.empty
        var childParents: Map[String, String] = Map.empty[String, String]

        for(line <- lines){
            if (line.startsWith("$ cd ..")){
                var parent = childParents(currentDir)
                currentDir = parent
            } else if (line.startsWith("$ cd")){
                var oldCurrentDir = currentDir
                currentDir = oldCurrentDir + "/" + line.substring(4).trim()
                allDirs += currentDir
            } else if (line.startsWith("$ ls")){
                // stuff
            } else if (line.startsWith("dir")){ 
                var dirName = line.substring(4).trim()
                var childOfCurrentDir = parentWithChildrenMap.getOrElse(currentDir, List.empty)
                childOfCurrentDir = childOfCurrentDir :+ (currentDir + "/" + dirName)
                parentWithChildrenMap = parentWithChildrenMap.updated(currentDir, childOfCurrentDir)
                allDirs += currentDir
                childParents = childParents + (currentDir + "/" + dirName -> currentDir)
            } else {                
                // starts with file size

                var fileInfo = line.split(" ")
                var fileSize = fileInfo(0)
                var fileName = fileInfo(1)
                sizesMap = sizesMap + (currentDir + "/" + fileName -> fileSize) 
                var parentList = parentWithChildrenMap.getOrElse(currentDir, List.empty)
                parentList = parentList :+ currentDir + "/" + fileName
                parentWithChildrenMap = parentWithChildrenMap.updated(currentDir, parentList)
                childParents = childParents + (currentDir + "/" + fileName -> currentDir)
            }
        }
        
        var sum = GetDirSize("//", parentWithChildrenMap, sizesMap)
        
        var currentEmptySpace = 70000000 - sum
        var spaceNeeded = 30000000 - currentEmptySpace

        var minSize = Int.MaxValue
        for(d <- allDirs){
            var sum = GetDirSize(d, parentWithChildrenMap, sizesMap)
            if(spaceNeeded - sum <= 0){
                if(sum < minSize){
                    minSize = sum
                }
            }
        }
        minSize
    }
}