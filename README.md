# connect5-swap

```
 ╭──────────────╮                                               
 │   connect5   │                                               
 │ ┌─┐┬ ┬┌─┐┌─┐ │                                               
 │ └─┐│││├─┤├─┘ │                                               
 │ └─┘└┴┘┴ ┴┴   │                                               
 ╰──────────────╯   
```

This application, called `connect5-swap`, will allow us to run a terminal version of a game based on the classic 2-player Connect4. It uses Haskell's [`brick`](https://github.com/jtdaugherty/brick/) library.

The original Connect4 uses a 6x7 board, but in this version, we will make it 8x10. There are 2 players, each playing with a different color, either red or yellow. Players alternate turns, and on each turn, the player drops their game piece into a column on the board. The piece will drop to the lowest position in the column that is not already occupied by another piece.

There are several twists in this version of the game:

- A player wins if they can get 5 consecutive pieces of their color into a row, column, or diagonal. In the original, a player only needs 4 consecutive pieces to win.
- The 5 consecutive pieces can wrap around the board. For example, ignoring all the other pieces on the board, R would have 5 consecutive pieces in each of these patterns: <br> 
```
    . . . . . . . . . .      . . . . . . . R . .      . . . . . . . . . .
    . . . . . . . . . .      . . . . . . . R . .      . . . . . . . . . R
    . . . . . . . . . .      . . . . . . . R . .      . . . . . . . . R .
    . . . . . . . . . .      . . . . . . . R . .      . . . . . . . . . .
    . . . . . . . . . .      . . . . . . . . . .      . . . . . . . . . .
    . . . . . . . . . .      . . . . . . . . . .      . . . . . R . . . .
    . . . . . . . . . .      . . . . . . . . . .      . . . . R . . . . .
    R R . . . . . R R R      . . . . . . . R . .      . . . R . . . . . .
```
- Most notably, the colors of the pieces on the board will be swapped every 5 rounds. All the red pieces will become yellow, and all the yellow pieces will become red. The players will continue to drop pieces of their original colors and will continue to try to get 5 consecutive pieces of that same color on the board to win.

## Prerequisites

You'll need to install:

- [haskell](https://www.haskell.org/platform/)
- [stack](https://docs.haskellstack.org/en/stable/README/)

## Playing `connect5-swap`

You can clone this repo and use `stack` to build and run like so:
```
git clone https://github.com/lim-rachel/connect5-swap.git
cd connect5-swap
stack build
stack run
```
