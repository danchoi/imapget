module Main where

import Text.ParserCombinators.Parsec  hiding (optional)
import Data.Either (either)
import Data.Serialize 
import Data.ByteString.Base64
import qualified Data.ByteString.Char8 as B
import Text.Regex.Posix
import Control.Monad (replicateM)
import Data.Maybe (catMaybes)
import Control.Applicative (optional)
import qualified Data.Binary as DB
import qualified Data.ByteString.UTF8 as UTF8
import Data.Char (chr)

xs = ["&AOkA6QDp-", "&bElbVw-", "&bElbVw-/&byJbVw-"]


eitherIntVal :: B.ByteString -> Either String [Integer]
eitherIntVal = runGet (do 
    xs <- replicateM 5 (optional getWord16be)
    return $ map fromIntegral $ catMaybes xs)

intVal :: B.ByteString -> [Integer]
intVal x = either error id (eitherIntVal x)

charNums :: String -> [Int]
charNums = map fromIntegral . intVal . decodeLenient . B.pack


plain :: GenParser Char st String
plain = many1 (noneOf "&")

special :: GenParser Char st String
special = do
  char '&'
  x <- many (noneOf "-")
  char '-'
  return $ (map chr . charNums)  x

word' = plain <|> special

parseUtf7Words :: GenParser Char st String
parseUtf7Words = fmap concat (many1 word')

parseUtf7 :: String -> Either ParseError String
parseUtf7 inp = parse parseUtf7Words "(unknown)" inp

main = do

    mapM_ (\x ->  do
        let r = either (error . show) id $ parseUtf7 x 
        putStrLn r
      ) xs

