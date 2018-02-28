module Main where
import qualified Text.Parsec as P
import Data.Functor.Identity(Identity)
import Control.Applicative((<|>))
import qualified Data.Char as C

import Lib

main :: IO ()
main = someFunc

group' :: Eq a => [a] -> [[a]]
group' [] = []
group' (x:xs) = insert' restxs x
  where insert' [] x = [[x]]
        insert' (xs:xxs) x | x `elem` xs = xs : insert' xxs x
                           | otherwise =  (x:xs) : xxs
        restxs = group' xs

data Number' = Int Int | Float Float deriving Show
data JSON = Null
  | Number Number'
  | String String
  | Bool Bool
  | Undefined
  | Object [(String ,JSON)]
  | List [JSON]
  deriving (Show)

parse :: String -> Either P.ParseError JSON
parse text = P.parse jsonParsec "JSON:" text


nullParsec :: P.Parsec String () JSON
nullParsec = P.string "null" >> return Null

stringParsec = do
  P.oneOf "\""
  x <- P.many $ P.noneOf "\""
  P.oneOf "\""
  return $ String x

stringParsec1 = do
  P.oneOf "\'"
  x <- P.many $ P.noneOf "\'"
  P.oneOf "\'"
  return $ String x
symbol1 = do
  P.spaces
  P.char ','
  P.spaces

listParsec = do
  P.char '['
  P.spaces
  a <- P.sepBy myParsec (P.try symbol1)
  P.spaces
  P.char ']'
  return $ List a

keyParsec :: P.Parsec String () String
keyParsec = do
  c <- P.lookAhead P.anyChar
  let val | C.isDigit c = fail "非法的key"
          | otherwise = P.many1 $ P.noneOf ": "
  val
objectInnerParsec = do
  (String key) <- stringParsec <|> stringParsec1 <|> (pure String <*> keyParsec) P.<?> "符合规定的key" 
  P.spaces
  P.char ':'
  P.spaces
  val <- myParsec
  return (key,val)
  
objectParsec = do
  P.char '{'
  P.spaces
  a <- P.sepBy objectInnerParsec (P.try symbol1)
  P.spaces
  P.char '}'
  return $ Object a

boolParsec = (Bool True <$ P.string "true") <|> (Bool False <$ P.string "false")

undefinedParsec = Undefined <$ P.string "undefined"

negDigit = pure (:)  <*> P.char '-' <*> posDigit
posDigit = P.many1 P.digit

negFloat = pure (:)  <*> P.char '-' <*> posFloat
posFloat = do
  digits <- P.many1 P.digit
  dot <- P.char '.'
  rdigits <- P.many1 P.digit
  return $ digits ++ (dot:rdigits)

digitParsec = Number . Int . (read :: String -> Int) <$> (posDigit <|> negDigit)
floatParsec = Number . Float . (read :: String -> Float) <$> (posFloat <|> negFloat)

myParsec = nullParsec
  <|> stringParsec
  <|> stringParsec1
  <|> listParsec
  <|> objectParsec 
  <|> boolParsec
  <|> undefinedParsec
  <|> floatParsec <|> digitParsec 

jsonParsec :: P.Parsec String () JSON
jsonParsec = P.spaces *> myParsec <* P.spaces <* P.eof 
