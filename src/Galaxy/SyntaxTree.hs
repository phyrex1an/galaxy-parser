module Galaxy.SyntaxTree where

data File = File Path [TopDeclaration]
            deriving Show

data TopDeclaration = VarDeclaration IsConst Type Identifier Statement
                    | NativeDeclaration Prototype
                    | FuncDeclaration IsStatic Prototype [Local] [TopStatement]
                    | Include Path
                      deriving Show
                      
data Prototype = Prototype Type Identifier [Argument]
               deriving Show

data Local = Local Type Identifier (Maybe Statement)
             deriving Show

data TopStatement = ReturnStatement (Maybe Statement)
                  | SetStatement Identifier Statement
                  | CallTopStatement Identifier [Statement]
                  | IfStatement If [If] (Maybe [TopStatement])
                  | While If
                  | Break
                  | Continue
                    deriving Show

data Statement = CallStatement Identifier [Statement]
               | VariableStatement Identifier
               | BinaryStatement Statement Op Statement
               | NegatedStatement Statement
               | NotStatement Statement
               | ValueStatement Value
                 deriving Show

data Value = StringValue String
           | FixedValue Rational
           | IntValue Integer
           | BoolValue Bool
           | NullValue
             deriving Show

data Op = Add | Sub | Mul | Div 
        | Lt | Lte | Eq | Nq | Gte | Gt
        | And | Or
        | BinAnd | BinOr
        | Mod
          deriving Show

type IsConst = Bool
type IsStatic = Bool
type Identifier = String
type Type = String
type Path = String
type Argument = (Type, Identifier)
type If = (Statement, [TopStatement])