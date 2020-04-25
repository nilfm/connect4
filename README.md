# connect4

This is an implementation in Haskell of the Connect 4 game

## Set-up

To install and play the game, follow these instructions:

If you don't have the `random` package installed, 
  
  * On Mac:
```bash
> brew install cabal-install
> cabal update
> cabal install --lib random
```
  * On Ubuntu:
```bash
> sudo apt install cabal-install
> cabal update
> cabal install random
```

To compile the game:

```bash
> ghc joc.hs
```

To execute the game:

```bash
> ./joc
```

## About the game

Connect 4 is a two-player game in which the players take turns dropping pieces into one of the columns in a rectangular grid. The objective of the game is to form a line (horizontal, vertical or diagonal) of length at least 4.

This particular implementation offers the user the possibility to choose a `width` and `height` for the board, as well as the computer's strategy. The strategies are explained in more detail in the following section. Note that if `width < 4` and `height < 4` the game will always end in a tie.

When you execute the game, you will be prompted to enter a width and height for the board. Then, you will be asked to enter a number between 1 and 3 to choose the opponent's strategy. In the case that your input is invalid for any reason, you will be notified and prompted again. After this process, the game starts.

The starting player is determined at random. Each player will then choose a column that is not full, and place a piece in that column. This process will go on until either the board is full or there is a winner.

## Strategies

### Random

The `Random` strategy is the simplest. It generates a list of all the column indices that are not full yet, and then it picks one at random.

### Greedy

The `Greedy` strategy always tries to get the longest possible chain with each play. In case of a tie, the column with the smallest index is chosen, except in cases where one play prevents the opponent from winning and the other doesn't. 

This strategy is easily counter-played because it is very predictable and can be tricked into bad decisions.

### Smart

The `Smart` strategy implements the minimax algorithm with alpha-beta pruning. 

The evaluation function used to assign a score to a board is the following: for each cell, calculate the longest chain that contains that cell and that still has a chance to become a 4-long chain. Then, if the piece at the cell is the computer's piece, add that to a running total. Otherwise, subtract it from the total.

This means that a n-long chain will be counted n times, for a total of n^2 points, but that is intentional since we want the longer chains to have more weight than the short ones. 

The minimax algorithm is applied with an amount of "look-ahead turns" that adapts to the current board: if `valid` is the amount of valid (not full) columns, the following formula is applied (with integer division): 

`turns = max(1, (9-valid)/2)`

This decision is made to think as far ahead as possible while respecting the time limits.

## Implementation details

I chose to implement the `Board` as a function of type `Ìnt -> Int -> Piece`, such that you give it a row and column and it returns the piece at that position. The types of piece are `Circle`, `Cross`, `None` and `OutOfBoard`.

The computer's decision is implemented via a higher order function, using the `Strategy` type, which is an alias for `Int -> Game -> IO Int`. This means that it receives a number (the current turn number) and a `Game` object and it returns an integer which represents the column number where it wants to place a piece. This integer is wrapped into `IO Int` because the `Random` strategy requires an IO action to get the random choice.

To get the user's input in a safe way, another higher order function is used: `readInt` is a function that takes a query string and a condition, in the form of an `Int -> Bool` function, and returns an `IO Int`. To deal with the cases where the user doesn't enter a number, the `readMaybe` function is used, which returns a `Maybe Int` which is `Nothing` if an invalid input is entered. Once we are sure that the user's input is an `Int`, we apply the condition to either ask again or return the input.