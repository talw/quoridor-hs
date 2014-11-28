### quoridor-hs

An implementation in Haskell of the 2-player strategy game

Work-in-progress.

# Features considered
- Online multiplayer
- Competent AI
- A CLI interface
- A web application

# Issues
- Can't see errors with hdevtools when editing quoridor.hs the main file.
    Started after adding a test-suite to the cabal file
- Have to repeat dependencies in build-depends on the test-suite
    because I can't add the main module in the build-depends of
    the test suite. Says it can't resolve package quoridor-hs by id
