module EvaLuAtor where 
import DataTypes 

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







