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

## Ray casting

- Start with list of consecutive polygon points
- Create tuples of neighbouring points, representing line segments
    - wrap around when you get to the end of the array
- Filter line segments for vertical lines only
- Obtain all intermediary points on the line, except for the very bottom point
    - we exclude the bottom point because *that's just how ray casting works*
    - Example: line segment [(3,5), (7,5)] becomes [(3,5), (4,5), (5,5), (6,5)] and [(4,70), (6,70)] becomes [(4,70), (5,70)]
- Encode the list of all intermediary points to a map 
    - key represents a row (x value)
    - value is a sorted array of columns in which the polygon border crosses it (y values)
    - The above example would look like: { 3: [5], 4: [5,70], 5: [5,70], 6: [5] }
- To ray cast a point: find the x row of the point in our encoded shape, and then see how many numbers are smaller than your y value
    - Point (4, 50)
        - map[4] is [5,70]
        - there is 1 number smaller than 50 so the ray casting number is 1
        - 1 is odd, so point IS WITHIN the polygon
    - Point (5, 100)
        - map[5] is [5,70]
        - there are 2 numbers smaller than 100 so the ray casting number is 2
        - 2 is even, so point IS NOT WITHIN the polygon
    - Note: Our example is incomplete, and just for illustrative purposes. The two vertical lines we have do not create an enclosed polygon; we need at least 2 more lines to fully describe a polygon
- If you are bigger than an odd number of y values: you are in the polygon. Otherwise, you are outside.

Optimization: sort all points by x and then y values and then do a reduce over them to create our encoded data structure
