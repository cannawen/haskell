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
- list comprehensions take 10 [2,4..] == [x*2 | x <- [1..10]]
