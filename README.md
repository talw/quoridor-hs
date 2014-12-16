# quoridor-hs

An implementation in Haskell of the 2-to-4-player strategy game

Work in progress.

### Install
I'm holding out on uploading this to Hackage,
as it's not polished enough.
Will upload once given the green light from
Haskellers much more experienced than I am.

So for now, to install:
~~~ {.bash}
$ git clone https://github.com/talw/quoridor-hs.git
$ cd quoridor-hs
$ cabal install
~~~

After which you should have a quoridor-exec in your .cabal/bin
or in dist/build/quoridor-exec/ where you 'cabal install'ed.

### TODO
- Features
    - Competent AI
    - A web application
- Features (DONE)
    - A CLI interface (cli interface is yet unpolished)
    - Online multiplayer
- Issues
- Issues (DONE)
    - DONE - Will have to move to a method where you calculate valid positions
      instead of getting a position and checking if it's valid.
      It will fix some edge cases.
    - DONE - Consider changing the rendering functions to return strings without
      causing IO. In which case use an efficient data structure to support
      the concatenations (difference lists?)
    - DONE - Consider adding some of the static configuration variables, like
      board size to a data type and using the ReaderT monad inside Game
    - DONE - dfs's visited list is made up for each sub traversal, instead of
      there being just a universal one. Might be able to be solved by
      sequencing the recursion with a state monad of the visited cells.
      Avoid pre-optimization though. Wait for profiling results
