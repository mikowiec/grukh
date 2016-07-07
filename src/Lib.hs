module Lib
    ( grokhMain
    ) where

import System.Environment

grokhMain :: IO ()
grokhMain = do
   putStrLn "someFunc"
   (command:argList) <- getArgs
   dispatch command argList


dispatch :: String -> [String] -> IO ()
dispatch "start" = cmdStart                      -- start tunnels by name from config file
dispatch "startAll" = cmdStartAll                -- start all tunnels defined in config file
dispatch "list" = cmdList                        -- list tunnel names from config file
dispatch "help" = cmdHelp                        -- view help
dispatch "version" = cmdVersion                  -- view version


cmdStart :: [String] -> IO ()
cmdStart [opts] = do
  putStrLn opts
  putStrLn "cmd"

cmdStartAll :: [String] -> IO ()
cmdStartAll [opts] = do
  putStrLn opts
  putStrLn "cmd"

cmdList :: [String] -> IO ()
cmdList [opts] = do
  putStrLn opts
  putStrLn "cmd"

cmdHelp :: [String] -> IO ()
cmdHelp [opts] = do
  putStrLn opts
  putStrLn "cmd"

cmdVersion :: [String] -> IO ()
cmdVersion [opts] = do
  putStrLn opts
  putStrLn "cmd"








