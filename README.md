### quoridor-hs

An implementation in Haskell of the 2-to-4-player strategy game

Work in progress.

# TODO
- Features
    - Online multiplayer
    - Competent AI
    - A web application
- Features (DONE)
    - A CLI interface
- Issues
    - Will have to move to a method where you calculate valid positions
      instead of getting a position and checking if it's valid.
      It will fix some edge cases.
- Issues (DONE)
    - DONE - Consider changing the rendering functions to return strings without
      causing IO. In which case use an efficient data structure to support
      the concatenations (difference lists?)
    - DONE - Consider adding some of the static configuration variables, like
      board size to a data type and using the ReaderT monad inside Game
    - DONE - dfs's visited list is made up for each sub traversal, instead of
      there being just a universal one. Might be able to be solved by
      sequencing the recursion with a state monad of the visited cells.
      Avoid pre-optimization though. Wait for profiling results
