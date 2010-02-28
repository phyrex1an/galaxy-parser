module Galaxy.Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Galaxy.SyntaxTree

import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( emptyDef )

lexer  = P.makeTokenParser 
         (emptyDef
         { 
--commentStart  :: String
--    Describes the start of a block comment. Use the empty string if the language doesn't support block comments. For example "/*". 
	P.commentStart = "",
--commentEnd :: String
--    Describes the end of a block comment. Use the empty string if the language doesn't support block comments. For example "*/". 
	P.commentEnd = "",
--commentLine :: String
--    Describes the start of a line comment. Use the empty string if the language doesn't support line comments. For example "//". 
	P.commentLine = "//",
--nestedComments :: Bool
--    Set to True if the language supports nested block comments. 
	P.nestedComments = False,
--identStart :: CharParser st Char
--    This parser should accept any start characters of identifiers. For example (letter <|> char "_"). 
	P.identStart = letter,
--identLetter :: CharParser st Char	
--    This parser should accept any legal tail characters of identifiers. For example (alphaNum <|> char "_"). 
	P.identLetter = alphaNum <|> oneOf "_",
--opStart :: CharParser st Char
--    This parser should accept any start characters of operators. For example (oneOf ":!#$%&*+./<=>?@\\^|-~") 
	P.opStart = oneOf "+-*/=><!|&",
--opLetter :: CharParser st Char
--    This parser should accept any legal tail characters of operators. Note that this parser should even be defined if the language doesn't support user-defined operators, or otherwise the reservedOp parser won't work correctly. 
	P.opLetter = oneOf "=&|",
--reservedNames :: [String]
--    The list of reserved identifiers. 
	P.reservedNames = ["const", "static", "while", "break", "continue", "if", "else"],
--reservedOpNames :: [String]
--    The list of reserved operators.
	P.reservedOpNames = [],
--caseSensitive :: Bool
--    Set to True if the language is case sensitive.
	P.caseSensitive = True 
	})

whiteSpace= P.whiteSpace lexer
lexeme    = P.lexeme lexer
symbol    = P.symbol lexer
natural   = P.natural lexer
parens    = P.parens lexer
braces    = P.braces lexer
semi      = P.semi lexer
comma     = P.comma lexer
identifier= P.identifier lexer
reserved  = P.reserved lexer
reservedOp= P.reservedOp lexer
semiSep1  = P.semiSep1 lexer
operator  = P.operator lexer
float     = P.float lexer
stringLiteral = P.stringLiteral lexer
naturalOrFloat= P.naturalOrFloat lexer


program :: String -> GenParser Char st File
program file = do
  whiteSpace
  ts <- many topDeclaration
  eof
  return $ File file ts

topDeclaration :: GenParser Char st TopDeclaration
topDeclaration = varDeclaration
                 <|> nativeDeclaration
                 <|> include
                 <|> funcDeclaration
    where
      varDeclaration = (do
        reserved "const"
        t <- identifier
        i <- identifier
        symbol "="
        s <- statement
        semi
        return $ VarDeclaration t i s
                       ) <?> "Variable declaration"
      nativeDeclaration = (do
        reserved "native"
        p <- prototype
        semi
        return $ NativeDeclaration p
                          ) <?> "Native"
      funcDeclaration = (do
        isStatic <- (reserved "static" >> return True) <|> return False
        p <- try prototype
        symbol "{"
        ls <-  many (try local)
        ts <- many topStatement
        symbol "}"
        return $ FuncDeclaration isStatic p ls ts
                        ) <?> "Function"
      include = (do
        reserved "include"
        p <- stringLiteral
        return $ Include p
               ) <?> "Include"

prototype :: GenParser Char st Prototype
prototype = (do 
  t <- identifier
  i <- identifier
  as <- parens (sepBy argument comma)
  return $ Prototype t i as
            ) <?> "Prototype"
    where
      argument = do
              t <- identifier
              i <- identifier
              return $ (t, i)

local :: GenParser Char st Local
local = (do
  t <- identifier
  i <- identifier
  v <- (fmap Just value) <|> return Nothing
  semi
  return $ Local t i v
        ) <?> "Local"

topStatement :: GenParser Char st TopStatement
topStatement = returnStatement
               <|> ifStatement
               <|> while
               <|> break
               <|> continue
               <|> try setStatement
               <|> (callStatement CallTopStatement >>= (\s -> semi >> return s))
    where
      returnStatement = (do
        reserved "return"
        s <- statement
        semi
        return $ ReturnStatement s
                        ) <?> "Return"
      setStatement = (do
        i <- identifier
        symbol "="
        s <- statement
        semi
        return $ SetStatement i s
                     ) <?> "Variable set"
      ifStatement = (do 
        reserved "if"
        (ts:elifs) <- sepBy1 ifExpr (reserved "else if")
        els <- (reserved "else" >> 
                         (fmap Just . braces $ many topStatement)) 
               <|> return Nothing
        return $ IfStatement ts elifs els
               ) <?> "If"
      while = (do
        reserved "while"
        b <- ifExpr
        return $ While b
               ) <?> "While"
      break = (do
        reserved "break"
        semi
        return Break
              ) <?> "Break"
      continue = (do
        reserved "continue"
        semi
        return Continue      
               ) <?> "Continue"
      ifExpr = do
        e <- parens statement
        b <- braces $ many topStatement
        return $ (e, b)
        

statement :: GenParser Char st Statement
statement = buildExpressionParser expressionTable terms
            <?> "Statement"
    where terms = valueStatement
		  <|> parens statement
                  <|> try (callStatement CallStatement)
                  <|> variableStatement
          variableStatement = do
            v <- identifier
            return $ VariableStatement v
          valueStatement = do
            v <- value
            return $ ValueStatement v

callStatement :: (Identifier -> [Statement] -> t) -> GenParser Char st t
callStatement f = do
        i <- identifier
        s <- parens (sepBy statement comma)
        return $ f i s
	
expressionTable :: OperatorTable Char st Statement
expressionTable = [ [prefix "-" NegatedStatement]
		  , [binary "*" Mul, binary "/" Div]
		  , [binary "+" Add, binary "-" Sub]
		  , [ binary ">" Gt
		    , binary ">=" Gte
		    , binary "==" Eq
		    , binary "!=" Nq
		    , binary "<=" Lte
		    , binary "<" Lt
                    , binary "|" BinOr
                    , binary "&" BinAnd
		    ]
		  , [prefix "!" NotStatement]
		  , [binary "&&" And, binary "||" Or]
                  ]
    where
      binary  name fun = Infix   (do{ reservedOp name; return $ (\a b -> BinaryStatement a fun b) }) AssocLeft
      prefix  name fun = Prefix  (do{ reservedOp name; return fun })

value :: GenParser Char st Value
value = stringValue 
        <|> number
        <|> boolValue
        <|> nullValue
    where
      stringValue = fmap StringValue stringLiteral
      number = do
        r <- naturalOrFloat
        case r of
          Left i -> return $ IntValue i
          Right d -> return . FixedValue $ toRational d
      boolValue = fmap BoolValue $ (reserved "true" >> return True)
                  <|> (reserved "false" >> return False)
      nullValue = reserved "null" >> return NullValue
           

doParse :: FilePath -> String -> Either ParseError File
doParse file input = runParser (program file) () file input