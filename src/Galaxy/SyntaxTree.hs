module Galaxy.SyntaxTree where

data File = File Path [TopDeclaration]

data TopDeclaration = VarDeclaration Type Identifier Value
                    | NativeDeclaration Prototype
                    | FuncDeclaration IsStatic Prototype [Local] [TopStatement]
                    | Include Path
                      
data Prototype = Prototype Type Identifier [Argument]

data Local = Local Type Identifier (Maybe Value)

data TopStatement = ReturnStatement Statement
                  | SetStatement Identifier Statement
                  | CallTopStatement Identifier [Statement]
                  | IfStatement If [If] (Maybe Statement)
                  | While Statement [TopStatement]
                  | Break
                  | Continue

data Statement = CallStatement Identifier [Statement]
               | VariableStatement Identifier
               | BinaryStatement Statement Op Statement

data Value = StringValue String
           | FixedValue Rational
           | IntValue Integer
           | BoolValue Bool
           | NullValue
           | Not Statement

data Op = Add | Sub | Mul | Div 
        | Lt | Lte | Eq | Gte | Gt
        | And | Or

type IsStatic = Bool
type Identifier = String
type Type = String
type Path = String
type Argument = (Type, Identifier)
type If = (Statement, [TopStatement])