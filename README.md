### quoridor-hs

An implementation in Haskell of the 2-player strategy game

Work-in-progress.

# TODO
- Features
    - Online multiplayer
    - Competent AI
    - A CLI interface
    - A web application
- Implementation details
    - Will have to move to a method where you calculate valid positions
      instead of getting a position and checking if it's valid.
      It will fix some edge cases.
    - Consider changing the rendering functions to return strings without
      causing IO. In which case use an efficient data structure to support
      the concatenations (difference lists?)
    - Consider adding some of the static configuration variables, like
      board size to a data type and using the ReaderT monad inside Game

# Issues
- Can't see errors with hdevtools when editing quoridor.hs the main file.
  Started after adding a test-suite to the cabal file
