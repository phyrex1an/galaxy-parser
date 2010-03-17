{-
This file is part of galaxy-parser.

galaxy-parser is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

galaxy-parser is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with galaxy-parser.  If not, see <http://www.gnu.org/licenses/>.
-}
module Galaxy.Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Galaxy.SyntaxTree
import Galaxy.Operators

import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( emptyDef )

import Control.Applicative ((<*>))

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
squares   = P.squares lexer
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
topDeclaration =  nativeDeclaration
                 <|> include
                 <|> typedef
                 <|> do
                   isStatic <- (reserved "static" >> return True) <|> return False
                   let funs = map ($ isStatic) [struct, constDeclaration, funcDeclaration, varDeclaration]
                   choice funs
                 <|> (semi >> topDeclaration)
                   
    where
      nativeDeclaration = (do
        reserved "native"
        p <- prototype
        semi
        return $ NativeDeclaration p
                          ) <?> "Native"
      include = (do
        reserved "include"
        p <- stringLiteral
        return $ Include p
               ) <?> "Include"
      typedef = (do
                  reserved "typedef"
                  t <- valueType
                  i <- identifier
                  semi
                  return $ TypeDef t i
                ) <?> "Typedef"
      struct isStatic = do
        reserved "struct"
        i <- identifier
        t <- braces (sepEndBy varDef semi)
        semi
        return $ Struct isStatic i t
      constDeclaration isStatic = (do
                           reserved "const"
                           v <- varDef
                           symbol "="
                           s <- statement
                           semi
                           return $ ConstDeclaration isStatic v s
                         ) <?> "Const"
      varDeclaration isStatic = (do
        v <- varDef
        s <- maybeSet
        semi
        return $ VariableDeclaration isStatic v s
                       ) <?> "Variable declaration"
      funcDeclaration isStatic = (do
        p <- try prototype
        b <- fmap Just functionBody <|> (semi >> return Nothing)
        return $ FuncDeclaration isStatic p b
                        ) <?> "Function"
      functionBody = braces $ do
        ls <- many (try local)
        ts <- many topStatement  
        return (ls, ts)

prototype :: GenParser Char st Prototype
prototype = (do 
  t <- valueType
  i <- identifier
  as <- parens (sepBy varDef comma)
  return $ Prototype t i as
            ) <?> "Prototype"

local :: GenParser Char st Local
local = do
  v <- varDef
  s <- maybeSet
  semi
  return $ LocalVar v s

topStatement :: GenParser Char st TopStatement
topStatement = returnStatement
               <|> ifStatement
               <|> while
               <|> break
               <|> continue
               <|> block
               <|> actionStatement
               <|> (semi >> topStatement)
    where
      returnStatement = (do
        reserved "return"
        s <- fmap Just statement <|> return Nothing
        semi
        return $ ReturnStatement s
                        ) <?> "Return"
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
      block = fmap Block (braces . many $ topStatement)
      actionStatement = do
        s <- statement
        semi
        return $ ActionStatement s
        

statement :: GenParser Char st Statement
statement = buildExpressionParser expressionTable terms
            <?> "Statement"
    where 
      terms = valueStatement
	      <|> parens statement
              <|> literalVariable
      literalVariable = do
        i <- identifier
        return $ VariableStatement (LiteralVariable i)
      valueStatement = do
        v <- value
        return $ ValueStatement v
      expressionTable :: OperatorTable Char st Statement
      expressionTable = concat [ 
                         [[ Postfix (do
                                     i <- squares statement
                                     return $ \s ->
                                         VariableStatement (ArrayDereference s i)
                                   )
                         , Postfix (do
                                     reservedOp "->"
                                     i <- identifier
                                     return $ \s ->
                                         VariableStatement (FieldPtrDereference s i)
                                   )
                         , Postfix (do
                                     reservedOp "."
                                     i <- identifier
                                     return $ \s ->
                                         VariableStatement (FieldDereference s i)
                                   )
                         , Postfix (do
                                     a <- parens (sepBy statement comma)
                                     return $ \s ->
                                         CallStatement s a
                                   )
                          ]]
                        , map2 unOp unaryOperators 
                        , map2 binOp binaryOperators
                        , map2 setOp assignementOperators 
                        ]                
      binOp  (name, sym) = Infix (do 
                                   reservedOp name
                                   return $ (\a b -> BinaryStatement a sym b)
                                  ) AssocLeft
      unOp   (name, sym) = Prefix (do
                                    reservedOp name
                                    return $ UnaryStatement sym
                                  )
      setOp  (name, sym) = Infix (do
                                   reservedOp name
                                   return $ \a b -> 
                                       AssignStatement a sym b
                                 ) AssocLeft
      map2 = map . map


value :: GenParser Char st Value
value = stringValue 
        <|> number
        <|> boolValue
        <|> nullValue
    where
      stringValue = fmap StringValue stringLiteral
      number = (do
        r <- naturalOrFloat
        case r of
          Left i -> return $ IntValue i
          Right d -> return $ FixedValue d
               ) <|> dotFloat     
      boolValue = fmap BoolValue $ (reserved "true" >> return True)
                  <|> (reserved "false" >> return False)
      nullValue = reserved "null" >> return NullValue
      dotFloat = do -- Not the prettiest code in town...
        char '.'
        d <- many1 digit
        return . FixedValue $ (read ('0':'.':d) :: Double)

valueType :: GenParser Char st Type
valueType = buildExpressionParser expressionTable terms
    where
      terms = plainType <|> parens valueType
      plainType =do
        i <- identifier
        return $ PlainType i
      expressionTable = [ [Postfix (do
                                     reservedOp "*"
                                     return PointerType
                                     ),
                           Postfix (do
                                     s <- many1 (squares statement)
                                     return $ \t ->
                                         foldl ArrType t s
                                   )
                          ] ]

varDef :: GenParser Char st VarDef
varDef = do
  t <- valueType
  i <- identifier
  return (t, i)

maybeSet :: GenParser Char st (Maybe Statement)
maybeSet = (symbol "=" >> statement >>= return . Just) <|> return Nothing

doParse :: FilePath -> String -> Either ParseError File
doParse file input = runParser (program file) () file input