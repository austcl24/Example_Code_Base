#!/usr/bin/env python3
import sys
from itertools import permutations

def main(argv):
    for line in sys.stdin:
        splitLine = line.strip().split(': ')
        buildingName = splitLine[0].strip()
        distancesC = splitLine[1].split(' ')
        distances = [int(x) for x in distancesC]
        combos = []
        distanceSet = {}
    
        origin = distances.index(0)
        buildingCount = len(distancesC)
    
        buildingList = [x for x in range(buildingCount)]
        # Add a second instance of the first building (0) so that possible
        # routes back to the first building are considered.
        buildingList.append(0)

        # Generate permuations of the building numbers that have all buildings
        # and start and end with the first building.
        combos.append([x for x in permutations(buildingList, buildingCount + 1)
             if x[0] == 0 and x[buildingCount] == 0])

        # Remove duplicate permutations caused by having two 'building zeros'.
        combos = list(set(combos[0]))
    
        # Build set of distances from input line's origin building to the other buildings
        for x in range(buildingCount):
            if (x != origin):
                workingKeyValue= str(origin) + "-" + str(x)
                distanceSet[workingKeyValue] = distances[x]
            
        # Take distanceSet and apply to each of the combinations. The reducer will sum up
        # all the input lines' contributions to each of the valid permuations of paths.
        for combo in combos:
            keyValue = '-'.join(map(str, combo))
            for x in range(buildingCount):
                workingKeyValue= str(combo[x]) + "-" + str(combo[x+1])
                partialDist = distanceSet.get(workingKeyValue)
                if (partialDist != None):
                # if we have a hit (should be only one per combo), generate intermediate key and value
                    outString = keyValue + "\tdistance:" + str(partialDist) + '|origin:' + str(origin)+'|name:'+buildingName
                    print(outString)

if __name__ == "__main__":
    main(sys.argv)
