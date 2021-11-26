# connect5-swap

This application, called "Connect5-Swap", will allow us to run a basic command-line version of a game based on the classic 2-player Connect4.

The premise of the game is that there is a 6x7 board, but in this version of the game, we will make it 7x9. There are 2 players, each playing with a different color, either red or yellow. Players alternate turns, and on each turn, they drop their game pieces into a certain column on the board. The piece will drop to the lowest position in the column that is not already occupied by another piece. In the original game, a player can win if they can get 4 consecutive pieces of their color into a row, column, or diagonally. In this version, players will have to get 5 consecutive pieces into a row, column, or diagonally.

One other twist that we will add is that every 5 rounds, the colors of the pieces on the board will be swapped. All the red pieces will become yellow, and all the yellow pieces will become red. The players will continue to drop pieces of their original colors and will continue to try to get 5 consecutive pieces of that same color on the board to win. This will make it more interesting and will require more planning on the part of the players.

Group members: Rachel Lim, Kyle Yang

## Milestone 2 Updates

- One of the key components is the game board. The game relies on the state of the board to figure out whose turn it is, to add the game pieces in the correct location, and to check if a player has won the game. It is important that a player is able to select a column to drop their piece and that it falls into the appropriate slot, and it is also important that the program can accurately identify whether a player has won the game or not.
- So far, the biggest challenge has been understanding how to use `brick`. It has taken a while to look through the starter code and comb through all the appropriate documentation. This has required a lot of time and patience, and it seems like after getting through that, things are starting to make a lot more sense now.
- The goals for this project still seem reasonable and can be met by the deadline.
