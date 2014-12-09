module Quoridor.Cmdline.Options (getOptions, Options(..), ExecMode(..))
where

import System.Console.GetOpt
import System.Exit (exitSuccess)
import Data.Maybe (fromMaybe)
import System.Environment (getProgName)

data Options = Options {
  opBoardSize :: Int,
  opGatesPerPlayer :: Int,
  opHostListenPort :: Int,
  opExecMode :: ExecMode
}

data ExecMode = ExLocal | ExHost | ExJoin
  deriving Eq

defaultOptions :: Options
defaultOptions = Options {
  opBoardSize = 9,
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

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "b" ["board-size"]
        (ReqArg
            (\arg opts -> return opts { opBoardSize = read arg })
            "INTEGER")
        "Board size (currently supports up to 9)"

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

    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                putStrLn (usageInfo prg options)
                exitSuccess))
        "Show help"
    ]
