# quoridor-hs

*For anyone wishing to look through the commits history, I apologize in advance for not using rebase -i to make sure I submit concise and meaningful commits only. This was really just a Haskell learning exercise for me. Having said that, I'll be happy to solve any unclarity regarding quoridor-hs.*

## Introduction

An implementation in Haskell of the 2-to-4-player strategy game.

Here's a description of the game's rules:
http://en.wikipedia.org/wiki/Quoridor#Rules_of_the_game

## Screenshots
### CLI
![CLI](https://cloud.githubusercontent.com/assets/1179015/5788108/d2323c36-9e2c-11e4-8ac5-56bd07b28207.png)
### Web browser interface
![Browser interface](https://cloud.githubusercontent.com/assets/1179015/5788107/c949078a-9e2c-11e4-909a-91b28c44acf5.png)

## Usage
quoridor-hs runs as a command-line application (quoridor-exec).

### Modes
The executable can be run in one of the following modes:
- Local play: The default mode (also flag -l), where all player play on the same terminal, specifying their moves in turn.
- Multiplayer
    - Host: with flag -h, the executable runs as a host of a game. Listening to connecting players. Port and ip           address can be specified in the format ADDR:Port, or only one of them can be specified without using              colons. In which case an attempt will be made to figure out if a port was given or an address. Defaults           are 127.0.0.1 and 33996.
        Even if prospective players do not have access or wish to use the executable, they can connect to the             server through the browser, as the host hosts an Http server as well. It's port can be set with -t.               Default is 33997.

    - Client: with flag -j, the executable is run as a client, to be connected to an executable that is running as       a host. IP address and a port are specified the same way as Host mode.

### Settings
Executing in local mode, or multiplayer's host mode, allows for the passage of arguments to specify game settings:
- Board size: -b, limited up to 9 rows and columns for now. (Just need to adjust the output to accomodate a ruler with numbers of 2 digits).
- Initial number of gates per player: -g
- Number of players: -n (2 to 4 players)

### Example usage
Starting a local game of 2 players, on a board of 9x9, with 10 gates.
~~~ {.bash}
./quoridor-exec
~~~
Starting a host on default address (localhost) and port 4545, with an http server listening on port 4646.
Game board size will be 7x7 and number of players is 3.
~~~ {.bash}
./quoridor-exec -b 7 -n 3 -h4545 -t4646
~~~

## Notes
- A multiplayer game will not begin until the required number of players joined. A player can disconnect before a     game begins, the remaining players will have to wait for an extra player.

- The game does not try to recover from a player leaving midgame (for now at least). Expect a crash.

- There is a chat panel which enables players to chat freely during game. Currently the chat is only available for     players using the browser interface. If and when the CLI interface will migrate to ncurses and the like, this     chat will be available there as well.

- Gates in quoridor are 2 in length, i.e. they block the path of 2 pairs of tiles. In order to facilitate game flow, a player is required to put the coordinates of the 2x2 square the wraps the 4 tiles and the 2 gates in question. In order to place the following horizontal gate (in a small piece of the game board) the player had to type g 6 3 v. g meaning the gate command, 6 3  being the x and y coordinates of the aforementioned square, and 'v' representing vertical (as opposed to h, horizontal).
```
    3 4
  6 x|x
  7 x|x
```


- Players using the CLI interface can play with players using the browser interface.

- Before you laugh at the 'command-line'y interface in the browser, let me defend my meager attempt by suggesting this was a learning exercise at general Haskell, not at client-side web development. As such, I wanted to maximize reuse of the CLI client, in the browser itself.
As such, when connecting in the browser to the host, the host actually spawns a CLI client, connecting its stdout and stdin to the Websockets port that communicates with the client. And the client's js simply dumps whatit receives on screen.

## Install

###Option 1: Install package from hackage###
~~~ {.bash}
$ mkdir quoridor-hs
$ cd quoridor-hs
$ cabal sandbox init
$ cabal install quoridor-hs
~~~

After which you should have a quoridor-exec in
.cabal-sandbox/bin/ where you 'cabal install'ed.

###Option 2: clone from Github###

To install:
~~~ {.bash}
$ git clone https://github.com/talw/quoridor-hs.git
$ cd quoridor-hs
$ cabal sandbox init
$ cabal install
~~~

After which you should have a quoridor-exec in
.cabal-sandbox/bin/ where you 'cabal install'ed.








## TODO
- Features
    - [ ] Competent AI
    - [x] A CLI interface
    - [x] Online multiplayer
    - [x] A browser interface
- Issues
    - [x] Should handle the case where a player disconnects
      not during midgame, but during waiting for the rest of the players
      to connect.
        - [x] For that, a function that sends a dummy message
          should be made. And using that function to test if
          the socket is alive.
          (Socket's IsConnected and IsReadable still return true
          even when the socket is dead)
    - [x] Should add a chat window
        - [x] Should recode the server so as to handle clients concurrently, rather than with one thread
              which will enable the addition of a chat window.
            - [ ] Will require changing the cli client to use a lib (ncurses?) to modify the terminal buffer, instead of
                  just printing more output
            - [x] The browser interface can still use the the older cli which outputs everything, but that older cli should
                  be modified that in browser proxy mode it also outputs the chat window updates, with some tag so that
                  it could be distinguished in javascript and displayed in a separate control.
    - [ ] Should add tests for render module and parsing module
    - [ ] Should try adding some QuickCheck tests, to try it.
    - [ ] Should use lens to manipulate and access GameState, to try it.
    - [ ] Add another distribution package without the http-server.
      It makes for a much smaller package for those not interested in that feature.
    - [x] Will have to move to a method where you calculate valid positions
      instead of getting a position and checking if it's valid.
      It will fix some edge cases.
    - [x] Consider changing the rendering functions to return strings without
      causing IO. In which case use an efficient data structure to support
      the concatenations (difference lists?)
    - [x] Consider adding some of the static configuration variables, like
      board size to a data type and using the ReaderT monad inside Game
    - [x] DFS's visited list is made up for each sub traversal, instead of
      there being just a universal one. Might be able to be solved by
      sequencing the recursion with a state monad of the visited cells.
      Avoid pre-optimization though. Wait for profiling results
