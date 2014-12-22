module Quoridor.Cmdline.Options
  ( Options(..)
  , ExecMode(..)
  , getOptions
  ) where

import           Control.Monad         (unless)
import           System.Environment    (getProgName)
import           System.Exit           (exitFailure, exitSuccess)

import           System.Console.GetOpt (ArgDescr (NoArg, OptArg),
                                        ArgDescr (ReqArg),
                                        ArgOrder (RequireOrder), OptDescr,
                                        OptDescr (Option), getOpt, usageInfo)

import           Quoridor.Helpers      (andP)

-- | Represents possible options from the cmdline
data Options = Options
  { opBoardSize      :: Int
  , opNumOfPlayers   :: Int
  , opGatesPerPlayer :: Int
  , opHostListenPort :: Int
  , opExecMode       :: ExecMode
  }

-- | Represents an execution mode for the program.
-- One can run quoridor at local play, server host or client join modes
data ExecMode = ExLocal | ExHost | ExJoin | ExProxy
  deriving Eq

defaultOptions :: Options
defaultOptions = Options
  { opBoardSize      = 9
  , opNumOfPlayers   = 2
  , opGatesPerPlayer = 10
  , opHostListenPort = 33996
  , opExecMode       = ExLocal
  }

-- exported functions

-- | Given the args from the cmdline,
-- returns them parsed into an Options data value.
-- It runs in the IO monad to allow the ability to exit the program
-- if something fails in the parsing (upon which, a usageInfo will be displayed)
getOptions :: [String] -> IO Options
getOptions args = do
  let (actions, _, _) = getOpt RequireOrder options args
  foldl (>>=) (return defaultOptions) actions



--helpers

isInRange :: Ord a => a -> a -> a -> Bool
isInRange a b c = ((>= b) `andP` (<= c)) a

putUsageInfoLn :: IO ()
putUsageInfoLn = do
  prg <- getProgName
  putStrLn (usageInfo prg options)

options :: [ OptDescr (Options -> IO Options) ]
options =
  [ Option "b" ["board-size"]
      (ReqArg
          (\arg opts -> do
            let argNum = read arg :: Int
            unless (isInRange argNum 2 9) $ do
              putUsageInfoLn
              exitFailure
            return opts { opBoardSize = argNum })
          "INTEGER")
      "Board size (2-9 rows/columns)"

  , Option "n" ["number-of-players"]
      (ReqArg
          (\arg opts -> do
            let argNum = read arg
            unless (isInRange argNum 2 4) $ do
              putUsageInfoLn
              exitFailure
            return opts { opNumOfPlayers = argNum })
          "INTEGER")
      "Number of players (2-4 players)"

  , Option "g" ["gates-per-player"]
      (ReqArg
          (\arg opts -> return opts { opGatesPerPlayer = read arg })
          "INTEGER")
      "Gates per player"

  , Option "l" ["local"]
      (NoArg $
          \opts -> return opts { opExecMode = ExLocal })
      "Start a local game"

  , Option "h" ["host"]
      (portOptionArg ExHost)
      "Host a game server"

  , Option "j" ["join"]
      (portOptionArg ExJoin)
      "Join a game server"

  , Option "p" ["client-proxy"]
      (portOptionArg ExProxy)
      "Client acts as proxy for a browser (used by the http server)"

  , Option "?" ["help"]
      (NoArg $
          \_ -> do
            putUsageInfoLn
            exitSuccess)
      "Show help"
  ]
 where portOptionArg execMode =
         OptArg
             (\arg opts -> return opts
                             { opExecMode = execMode
                             , opHostListenPort =
                                 maybe (opHostListenPort opts) read arg
                             })
             "PORT"

