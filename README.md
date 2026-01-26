Resoruces

1. https://learnyouahaskell.github.io/introduction.html
2. https://haskell.mooc.fi/part1
3. https://www.youtube.com/playlist?list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV


Learnings
- function calls, infix functions
- list manipulation (++ : !!)
    - head, tail, init, last
    - length, null, reverse, take, drop, maximum, minimum, sum, product, elem
- ranges [1..], ['K'..'Z']
    - steps, [2, 4, 6, 8, 10] == [2, 4..10], == take 5 [2,4..]
    - cycle, repeat, replicate
        - take 10 (cycle "LOL ") == "LOL LOL LO"
        - take 10 (repeat 5) == [5,5,5,5,5,5,5,5,5,5] 
        - replicate 3 10 == [10,10,10]. 
- overall: [ output_expression | binding(s), filter1, filter2, ... ]
    - list must all be of the same kind
    - list comprehensions take 10 [2,4..] == [x*2 | x <- [1..10]] == [2,4,6,8,10,12,14,16,18,20]
    - predicates (filter) [x*2 | x <- [1..10], x*2 >= 12]  == [12,14,16,18,20]
    - multiple bindings [ x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]  == [55,80,100,110]  
- tuple: 2 or more values in ()
    - known number of variables in a list
    - may be of different types
    - fst, snd, zip
        - zip [5,3,2,6,2,7,2,5,4,6,6] ["im","a","turtle"]  == [(5,"im"),(3,"a"),(2,"turtle")]
- all together now, finding right angle triangle with lengths < 10 and perimeter 24
    - [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24] 
