#!/usr/bin/env python3
import sys

def main(argv):
    current_word = None
    current_count = 0
    word = None
    outPath = []

    for line in sys.stdin:
    # break apart an intermediate key that is formatted like:
    # '0-1-2-4-3-0\tdistance:13|origin:3|name:Building3'

        line = line.strip()
        word, valueString = line.split('\t', 1)
        valueList = valueString.split('|',2)

        stops = [int(x) for x in word.split('-')]

        distanceList = valueList[0].split(':',1)
        count = int(distanceList[1])

        originList = valueList[1].split(':', 1)
        origin = int(originList[1])

        nameList = valueList[2].split(':', 1)
        buildingName = nameList[1]

        if current_word == word:
            current_count += count
            outList[stops.index(origin)] = buildingName
        else:
            if current_word:
                outString = ' '.join(outList)
                outString = outString + ' ' + outList[0]
                print('%s : %s' % (current_count, outString))
            else:
                outList = [None] * (len(stops) - 1)
            current_count = count
            current_word = word
            outList[stops.index(origin)] = buildingName

    if current_word == word:
        outString = ' '.join(outList)
        outString = outString + ' ' + outList[0]
        print('%s : %s' % (current_count, outString))

if __name__ == "__main__":
    main(sys.argv)
