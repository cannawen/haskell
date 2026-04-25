# Problem

[AoC 2025 Day 9 Pt 2](https://adventofcode.com/2025/day/9)

Given a list of consecutive points constructing a polygon, find the largest rectangle (using 2 of the given points) that can fit within the polygon.

# Solution

## High level

- Find all possible rectangles
- Sort by largest to smallest area
- Filter for rectangles where all 4 corners are within polygon
    - performace optimization (haskell will lazily filter list)
- Find the first rectangle where the entire perimeter is within the polygon

A point counts as "within" the polygon if one of two conditions hold:
- The point is on a border, or
- The point crosses an odd number of lines ([ray casting](https://en.wikipedia.org/wiki/Point_in_polygon#Ray_casting_algorithm))

## Check if a point is on a border

- Start with list of consecutive polygon points
- Create tuples of neighbouring points, representing line segments
- Create a set all intermediary points on the line
- Check if point is a member of the set

## Ray casting

Given a non-border point and an encoded polygon data structure, we want to see if the point is within the polygon.

### Calculating the encoded polygon data structure

- Start with list of consecutive polygon points
- Create tuples of neighbouring points, representing line segments
- Filter line segments for vertical lines only
- Obtain all intermediary points on the line, except for the very bottom point
    - we exclude the bottom point because *that's just how ray casting works*
    - Example: 
        - line segment [(3,5), (6,5)] becomes [(3,5), (4,5), (5,5)]
        - line segment [(3,70), (6,70)] becomes [(3,70), (4,70), (5,70)]
- Encode the list of all intermediary points to a map, where: 
    - key represents a row (x value)
    - value is a sorted array of columns in which the polygon border crosses it (y values)
    - The above example would look like: { 3: [5,70], 4: [5,70], 5: [5,70] }

Optimization: sort all points by x and then y values and then do a reduce over them, looking at the previous entry in the memo to see if the x has not changed we should append to an existing [y] array, or if the x has changed if we should construct a new x key in the map

### Seeing if a point is within the polygon

- Given (px,py) point, find map[px] of our encoded shape to get [y] of the polygon
- See how many [y] are smaller than py. This is how many lines your ray crosses.
- If you count an odd number: you are inside the polygon. Otherwise, you are outside.
- Example
    - is (3,50) within our polygon?
        - map[3] is [5,70]
        - there is 1 number smaller than 50 so the ray casting number is 1
        - 1 is odd, so point *is within* the polygon
    - is (4,100) within our polygon?
        - map[4] is [5,70]
        - there are 2 numbers smaller than 100 so the ray casting number is 2
        - 2 is even, so point *is not within* the polygon
    - is (5,3) within our polygon?
        - map[5] is [5,70]
        - there are 0 numbers smaller than 3 so the ray casting number is 0
        - 0 is even, so point *is not within* the polygon
    - is (6,50) within our polygon?
        - map[100] is Nothing
        - there are 0 numbers smaller than 100 so the ray casting number is 0
        - 0 is even, so point *is not within* the polygon
    - **BUT WAIT, (6,50) IS WITHIN OUR POLYGON!!!**
        - This is where "Check if a point is on a border" will return true in our if statement
        - The ray tracing is only accurate for non-border points
