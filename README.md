# quoridor-hs

An implementation in Haskell of the 2-to-4-player strategy game.

Here's a description of the game's rules:
http://en.wikipedia.org/wiki/Quoridor#Rules_of_the_game

Work in progress.
Currently supports local or multiplayer over TCP.
Client is either CLI or a browser page which is hooked to a client CLI process
on the server (i.e. complete reuse of the CLI interface for the browser page).
Browser page is served via http server that the executable runs if run with --host flag



### Install
I'm holding out on uploading this to Hackage,
as it's not polished enough.
Will upload once given the green light from
Haskellers much more experienced than I am.

To install:
~~~ {.bash}
$ git clone https://github.com/talw/quoridor-hs.git
$ cd quoridor-hs
$ cabal sandbox init
$ cabal install
~~~

After which you should have a quoridor-exec in
dist/build/quoridor-exec/ where you 'cabal install'ed.

### TODO
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
    - [ ] Should add a chat window
        - [ ] Should recode the server so as to handle clients concurrently, rather than with one thread
              which will enable the addition of a chat window.
            - [ ] Will require changing the cli client to use a lib (ncurses?) to modify the terminal buffer, instead of
                  just printing more output
            - [ ] The browser interface can still use the the older cli which outputs everything, but that older cli should
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
