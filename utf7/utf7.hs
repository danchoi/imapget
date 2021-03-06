module Main where

import Text.ParserCombinators.Parsec hiding (optional)
import Data.Either (either)
import Data.Serialize (runGet, getWord16be)
import Data.ByteString.Base64 (decodeLenient)
import qualified Data.ByteString.Char8 as B
import Control.Monad (replicateM)
import Data.Maybe (catMaybes)
import Control.Applicative (optional)
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

-- parsec functions

plain :: GenParser Char st String
plain = many1 (noneOf "&")

special :: GenParser Char st String
special = do
  char '&'
  x <- many (noneOf "-")
  char '-'
  return $ (map chr . charNums)  x

utf7Letter = plain <|> special

utf7Letters :: GenParser Char st String
utf7Letters = fmap concat (many1 utf7Letter)

parseUtf7 :: String -> Either ParseError String
parseUtf7 = parse utf7Letters "(unknown)" 

main = do

    mapM_ (\x ->  do
        let r = either (error . show) id $ parseUtf7 x 
        putStrLn r
      ) xs

