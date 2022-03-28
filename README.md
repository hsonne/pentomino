# pentomino

The script `pentomino.R`, written in the programming language "R", can be used 
to find the solutions of the Pentomino puzzle.

# Pentomino: My Favourite Puzzle

The Pentomino puzzle consists of twelve unique parts, each of which is 
composed of five unit squares. I gave each part the name of a letter:

```
I: █  L: █    Y: █    V: █     T: █ █ █  X:  █      
   █     █       █ █     █          █      █ █ █
   █     █       █       █ █ █      █        █
   █     █ █     █
   █

M: █ █    E: █ █     U: █   █  K:  █ █  P: █ █  S:  █ █ 
     █ █       █ █ █    █ █ █    █ █       █ █      █
       █                           █       █      █ █
```

The task is to puzzle the parts together into a rectangular field with a size
of 12 * 5 = 60 unit squares, i.e. sized 3 x 15, 5 x 12, 6 x 10.

The following script finds all solutions for the 6 x 10 rectangle. 