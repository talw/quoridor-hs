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
  , opHttpListenPort :: Int
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
  , opHttpListenPort = 33997
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
            argNum <- rangedOption 2 9 arg
            return opts { opBoardSize = argNum })
          "INTEGER")
      "Board size (2-9 rows/columns). default 9"

  , Option "n" ["number-of-players"]
      (ReqArg
          (\arg opts -> do
            argNum <- rangedOption 2 4 arg
            return opts { opNumOfPlayers = argNum })
          "INTEGER")
      "Number of players (2-4 players). default 2"

  , Option "g" ["gates-per-player"]
      (ReqArg
          (\arg opts -> do
            argNum <- rangedOption 0 100 arg
            return opts { opGatesPerPlayer = argNum })
          "INTEGER")
      "Gates per player (1-100 gates per player). default 10"

  , Option "l" ["local"]
      (NoArg $
          \opts -> return opts { opExecMode = ExLocal })
      "Start a local game"

  , Option "h" ["host"]
      (portOptionArg ExHost)
      "Host a game server. default port 33997"

  , Option "j" ["join"]
      (portOptionArg ExJoin)
      "Join a game server"

  , Option "p" ["client-proxy"]
      (portOptionArg ExProxy)
      "Client acts as proxy for a browser (used by the http server)"

  , Option "t" ["http-port"]
      (ReqArg
          (\arg opts -> do
            argNum <- rangedOption 1025 65535 arg
            return opts { opHttpListenPort = argNum })
          "PORT")
      "A port for the http-server (port 1025-65535), relevant only if --host flag is used. default 33997"

  , Option "?" ["help"]
      (NoArg $
          \_ -> do
            putUsageInfoLn
            exitSuccess)
      "Show help"
  ]
 where portOptionArg execMode =
         OptArg
             (\arg opts -> do
                argNum <- maybe (return $ opHostListenPort opts)
                  (rangedOption 1025 65535) arg
                return opts
                  { opExecMode = execMode
                  , opHostListenPort = argNum
                  })
              "PORT"
       rangedOption x y arg = do
         let argNum = read arg
         unless (isInRange argNum x y) $ do
           putUsageInfoLn
           exitFailure
         return argNum
