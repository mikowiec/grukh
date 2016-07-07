-- {-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where

import Data.Aeson
import qualified Data.Aeson.Types as T
import Lib
--import Options.Applicative
--import Control.Monad
import Data.Text
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Control.Monad     --(mzero)
import Data.Attoparsec (parse, Result(..))
import Data.Attoparsec.ByteString
import Control.Applicative ((<$>))


-- Types
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
             in case parse json bs of
                  (Done rest r) -> T.parseMaybe parseJSON r :: Maybe Msg
                  _             -> Nothing



main :: IO ()
main = do
  print $ parseMsg exampleJSONMessage
  let reply = Msg "It's Aeson!"
  putStrLn $ "Encoded: " ++ (BSL.unpack (encode reply))
  grokhMain

-- main = do
-- execcParser (info parser fullDesc)
--  (uncurry cmdExec)






