module Json where

import qualified Parser as P
import Prelude hiding (null)
import Result

data Json
  = JNull
  | JBool Bool
  | JInt Int
  | JString String
  | JList [Json]
  | JDict [(String, Json)]
  deriving (Show)

value :: P.Parser Json
value =
  P.pMap (const JNull) null
    `P.orElse` P.pMap JBool bool
    `P.orElse` P.pMap JInt int
    `P.orElse` P.pMap JString string
    `P.orElse` P.pMap JList list
    `P.orElse` P.pMap JDict dict

-- >>> P.runParser null "null"
-- Success (()),"")
null :: P.Parser ()
null = P.string "null" `P.pThen` P.succeed () `P.expecting` "null"

-- >>> P.runParser bool "true"
-- Success (True,"")
--
-- >>> P.runParser bool "false"
-- Success (False,"")
bool :: P.Parser Bool
bool = pTrue `P.orElse` pFalse
  where
    pTrue = P.string "true" `P.pThen` P.succeed True `P.expecting` "true"
    pFalse = P.string "false" `P.pThen` P.succeed False `P.expecting` "false"

-- >>> P.runParser int "123"
-- Success (123,"")
int :: P.Parser Int
int = P.number

-- >>> P.runParser string  "\"abc\""
--  Success ("abc","")
string :: P.Parser String
string = P.between (P.char '"') (P.char '"') (P.many (P.satisfies (/= '"') "quote '\"'")) `P.expecting` "string"

-- >>> P.runParser list "[1,2,3]"
-- Success ([JInt 1,JInt 2,JInt 3],"")
list :: P.Parser [Json]
list = P.between (P.char '[') (P.char ']') (P.sepBy (P.char ',') value) `P.expecting` "list"

-- >>> P.runParser dict "{\"a\":1,\"b\":2}"
-- Success ([("a",JInt 1),("b",JInt 2)],"")
dict :: P.Parser [(String, Json)]
dict = P.between (P.char '{') (P.char '}') (P.sepBy (P.char ',') kv) `P.expecting` "dict"
  where
    kv = string `P.andThen` ((P.char ':') `P.pThen` value)

parse :: String -> Result P.ParseError Json
parse s = fst <$> P.runParser value s
