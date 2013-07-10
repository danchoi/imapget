module Main where
import Data.Either (either)
import Data.Serialize
import Data.ByteString.Base64
import qualified Data.ByteString.Char8 as B
import Text.Regex.Posix
import Control.Monad (replicateM)
import Data.Maybe (catMaybes)
import Control.Applicative ((<|>))

xs = ["&AOkA6QDp-", "&bElbVw-", "&bElbVw-/&byJbVw-"]


eitherIntVal :: B.ByteString -> Either String [Integer]
eitherIntVal = runGet (do 
    xs <- replicateM 5 (Just `fmap` getWord16be <|> return Nothing)
    return $ map fromIntegral $ catMaybes xs)



intVal :: B.ByteString -> [Integer]
intVal x = either error id (eitherIntVal x)

charNums :: String -> [Integer]
charNums = intVal . decodeLenient . B.pack

-- divides utf7 by word dividers 
divide :: String -> [String]
divide x = map shave $ getAllTextMatches $ x =~ "&([^-]*)-" :: [String]
    where shave s = drop 1 $ take (length s - 1) s 

conv :: String -> [[Integer]]
conv s = map (charNums) $ divide s

main = do
    mapM_ (putStrLn.show. divide) xs
    mapM_ (putStrLn.show. (map charNums) .divide) xs

