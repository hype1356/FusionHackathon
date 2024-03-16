import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import DataTypes 
import Control.Monad 



-- symbols that are allowed 
symbol:: Parser Char 
symbol = oneOf "!$%&|*+-/:<>=?@^_#~"


-- reads the string 
readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of 
    Left err -> String $ "No match: " ++ show err 
    Right val -> val 


-- main method 
main:: IO()
main = do 
    getArgs >>= putStrLn . show . eval . readExpr . (!!0)


-- ignored spaces 
spaces :: Parser()
spaces = skipMany1 space 


-- parses strings 
parseString :: Parser LispVal
parseString = do 
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ String x 


-- parses LISP Atom 
parseAtom:: Parser LispVal
parseAtom = do 
    first <- letter <|> symbol 
    rest <- many (letter <|> digit <|>  symbol)
    let atom = [first] ++ rest 
    return $ case atom of 
        "#t" -> Bool True 
        "#f" -> Bool False 
        otherwise -> Atom atom 



--  parsers numbers 
parseNumbers :: Parser LispVal 
parseNumbers = liftM (Number . read) $ many1 digit 


-- parses Expressions 
parseExpr:: Parser LispVal 
parseExpr = parseAtom 
    <|> parseString
    <|> parseNumbers
    <|> parseQuoted
    <|> do 
        char '(' 
        x <- try parseList <|> parseDottedList
        char ')'
        return x  



-- parses LISP style infamous lists 
parseList:: Parser LispVal 
parseList = liftM List $ sepBy parseExpr spaces 



-- parses Scheme dotted lists 
parseDottedList:: Parser LispVal
parseDottedList = do 
    head <- endBy parseExpr spaces 
    tail <- char '.' >> spaces >> parseExpr 
    return $ DottedList head tail 

-- single quoted syntax sugar of Scheme 
parseQuoted:: Parser LispVal
parseQuoted = do 
    char '\'' 
    x <- parseExpr
    return $ List [Atom "quote", x]


-- instance of show for the LispVal datatype 
instance Show LispVal where show :: LispVal -> String
                            show = showVal





showVal :: LispVal -> String 
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name 
showVal (Number contents) = show contents 
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ "." ++ showVal tail ++ ")"


unwordsList:: [LispVal] -> String 
unwordsList = unwords . map showVal



-- simple Scheme styples primitives evaluator 
eval:: LispVal -> LispVal
eval value@(String _) = value 
eval value@(Bool _) = value 
eval value@(Number _) = value 
eval (List [Atom "quote", val]) = val 
eval (List (Atom func : args)) = apply func $ map eval args 


apply:: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives 


primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)
             ]


numericBinop:: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNums params


unpackNums:: LispVal -> Integer
unpackNums (Number n) = n 
unpackNums (String n) = let parsed = reads n in 
                            if null parsed 
                                then 0 
                                else fst $ parsed !! 0 

unpackNums (List [n]) = unpackNums n 
unpackNums _ = 0 







