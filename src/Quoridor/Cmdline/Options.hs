module Quoridor.Cmdline.Options (getSettings, Settings(..))
where

import System.Console.GetOpt
import System.Exit (exitSuccess)
import Data.Maybe (fromMaybe)
import System.Environment (getProgName)

data Settings = Settings {
  boardSize :: Int,
  gatesPerPlayer :: Int,
  isLocal :: Bool,
  isHost :: Bool,
  hostStartPort :: Int,
  isJoin :: Bool,
  joinPorts :: [Int]
}

defaultSettings :: Settings
defaultSettings = Settings {
  boardSize = 9,
  gatesPerPlayer = 10,
  isLocal = True,
  isHost = False,
  hostStartPort = 33996,
  isJoin = False,
  joinPorts = [33996,33997,33998,33999]
}

-- exported functions

getSettings :: [String] -> IO Settings
getSettings args = do
  let (actions, nonOpts, msgs) = getOpt RequireOrder options args
  foldl (>>=) (return defaultSettings) actions



--helpers

options :: [ OptDescr (Settings -> IO Settings) ]
options =
    [ Option "b" ["board-size"]
        (ReqArg
            (\arg settings -> return settings { boardSize = read arg })
            "INTEGER")
        "Board size (currently supports up to 9)"

    , Option "g" ["gates-per-player"]
        (ReqArg
            (\arg settings -> return settings { gatesPerPlayer = read arg })
            "INTEGER")
        "Gates per player"

    , Option "l" ["local"]
        (NoArg
            (\settings -> return settings {
              isLocal = True, isHost = False, isJoin = False }))
        "Start a local game"

    , Option "h" ["host"]
        (OptArg
            (\arg settings -> return settings {
              isHost = True, isLocal = False, isJoin = False,
              hostStartPort = maybe (hostStartPort settings) read arg })
              "PORT")
        "Host a game server"

    , Option "j" ["join"]
        (OptArg
            (\arg settings -> return settings {
              isHost = True, isLocal = False, isJoin = False,
              joinPorts = maybe (joinPorts settings) (return . read) arg })
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
