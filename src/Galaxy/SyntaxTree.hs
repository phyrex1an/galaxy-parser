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
module Galaxy.SyntaxTree where

data File = File Path [TopDeclaration]
            deriving Show

data TopDeclaration = VarDeclaration IsConst Type Identifier Statement
                    | ArrDeclaration Type Size Identifier
                    | NativeDeclaration Prototype
                    | FuncDeclaration IsStatic Prototype (Maybe FunctionBody)
                    | Include Path
                      deriving Show
                      
data Prototype = Prototype Type Identifier [Argument]
               deriving Show

data Local = LocalVar Type Identifier (Maybe Statement)
           | LocalArr Type Size Identifier
             deriving Show

data TopStatement = ReturnStatement (Maybe Statement)
                  | SetStatement Identifier Statement
                  | SetArrayStatement Identifier Statement Statement
                  | CallTopStatement Identifier [Statement]
                  | IfStatement If [If] (Maybe [TopStatement])
                  | While If
                  | Break
                  | Continue
                    deriving Show

data Statement = CallStatement Identifier [Statement]
               | VariableStatement Identifier
               | ArrayStatement Identifier Statement
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

type Size = Integer
type IsConst = Bool
type IsStatic = Bool
type Identifier = String
type Type = String
type Path = String
type Argument = (Type, Identifier)
type If = (Statement, [TopStatement])
type FunctionBody = ([Local], [TopStatement])