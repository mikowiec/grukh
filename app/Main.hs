-- {-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where

import Data.Aeson
import qualified Data.Aeson.Types as T
import Lib
--import Options.Applicative
--import Control.Monad
-- import Data.Text
import Data.Text hiding (foldl, concat)
import Data.Foldable hiding (foldl)
-- import Prelude hiding (foldl)
--
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Control.Monad (mzero)
import
  qualified Data.Attoparsec as ATTO (parse, Result(..))
import qualified Data.Attoparsec.ByteString as ATTO
import Control.Applicative ((<$>))
-- cmd line parser
import System.Exit
import System.Console.GetOpt
import System.Environment ( getArgs )
import Data.Maybe ( fromMaybe )




-- Types JSON
data Msg = Msg Text deriving (Show)


-- parse JSON and Msg constructor
instance FromJSON Msg where
  parseJSON (Object v) = Msg <$> v .: "message"
  parseJSON _ = mzero

instance ToJSON Msg where
  toJSON (Msg s) = object [ "message" .= s]


-- {"message": "hello world"}
exampleJSONMessage :: String
exampleJSONMessage = "{\"message\":\"hello world\"}"


parseMsg :: String -> Maybe Msg
parseMsg s = let bs = BS.pack s
             in case ATTO.parse json bs of
                  (ATTO.Done rest r) -> T.parseMaybe parseJSON r :: Maybe Msg
                  _                  -> Nothing


-- ==========================
-- cmd parser


-- Types cmd line
data Options = Options {
    optInput :: IO String
  , optOutput :: String -> IO ()
  }


grkDefaultOptions :: Options
grkDefaultOptions = Options {
    optInput = getContents
  , optOutput = putStr
  }


options :: [OptDescr (Options -> IO Options)]
options =
  [
    Option ['V'] ["version"] (NoArg showVersion) "show version number"
  , Option ['i'] ["input"] (ReqArg readInput "FILE") "input file to read"
  , Option ['o'] ["output"] (ReqArg writeOutput "FILE") "output file to write"
  ]


showVersion _ = do
  putStrLn "CommandLine example 0.1"
  exitSuccess


readInput arg opt = return opt { optInput = readFile arg }

writeOutput arg opt = return opt { optOutput = writeFile arg }


main :: IO ()
main = do
  args <- getArgs
  print $ show args
  --
  let ( actions, nonOpts, msgs ) = getOpt RequireOrder options args
  if msgs == []
    then do
      print "aa"
    else do
      let header = "\nUsage: "
      print $ concat msgs ++ usageInfo header options
      exitFailure
  --exitSuccess
  opts <- foldl (>>=) (return grkDefaultOptions) actions
  let Options {
        optInput = input
      , optOutput = output
    } = opts
  input >>= output
  --
  print $ parseMsg exampleJSONMessage
  let reply = Msg "It's Aeson!"
  putStrLn $ "Encoded: " ++ (BSL.unpack (encode reply))
  grokhMain

-- main = do
-- execcParser (info parser fullDesc)
--  (uncurry cmdExec)






