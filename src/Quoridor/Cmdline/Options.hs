module Quoridor.Cmdline.Options (getOptions, Options(..), ExecMode(..))
where

import Quoridor.Helpers (andP)
import System.Console.GetOpt
import System.Exit (exitSuccess, exitFailure)
import Data.Maybe (fromMaybe)
import System.Environment (getProgName)
import Control.Monad

data Options = Options {
  opBoardSize :: Int,
  opNumOfPlayers :: Int,
  opGatesPerPlayer :: Int,
  opHostListenPort :: Int,
  opExecMode :: ExecMode
}

data ExecMode = ExLocal | ExHost | ExJoin
  deriving Eq

defaultOptions :: Options
defaultOptions = Options {
  opBoardSize = 9,
  opNumOfPlayers = 2,
  opGatesPerPlayer = 10,
  opHostListenPort = 33996,
  opExecMode = ExLocal
}

-- exported functions

getOptions :: [String] -> IO Options
getOptions args = do
  let (actions, nonOpts, msgs) = getOpt RequireOrder options args
  foldl (>>=) (return defaultOptions) actions



--helpers

isInRange :: Ord a => a -> a -> a -> Bool
isInRange a b c = ((>= b) `andP` (<= c)) a

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ let optText = "Board size (2-9 rows/columns)"
      in Option "b" ["board-size"]
        (ReqArg
            (\arg opts -> do
              let argNum = read arg :: Int
              unless (isInRange argNum 2 9) $ do
                putStrLn optText
                exitFailure
              return opts { opBoardSize = argNum })
            "INTEGER")
        optText

    , let optText = "Number of players (2-4 players)"
      in Option "n" ["number-of-players"]
        (ReqArg
            (\arg opts -> do
              let argNum = read arg
              unless (isInRange argNum 2 4) $ do
                putStrLn optText
                exitFailure
              return opts { opNumOfPlayers = argNum })
            "INTEGER")
        optText

    , Option "g" ["gates-per-player"]
        (ReqArg
            (\arg opts -> return opts { opGatesPerPlayer = read arg })
            "INTEGER")
        "Gates per player"

    , Option "l" ["local"]
        (NoArg
            (\opts -> return opts { opExecMode = ExLocal }))
        "Start a local game"

    , Option "h" ["host"]
        (OptArg
            (\arg opts -> return opts { opExecMode = ExHost,
              opHostListenPort = maybe (opHostListenPort opts) read arg })
              "PORT")
        "Host a game server"

    , Option "j" ["join"]
        (OptArg
            (\arg opts -> return opts { opExecMode = ExJoin,
              opHostListenPort = maybe (opHostListenPort opts) read arg })
              "PORT")
        "Join a game server"

    , Option "?" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                putStrLn (usageInfo prg options)
                exitSuccess))
        "Show help"
    ]
