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
            

data TopDeclaration = ConstDeclaration IsStatic VarDef Statement
                    | VariableDeclaration IsStatic VarDef (Maybe Statement)
                    | TypeDef Type Identifier
                    | NativeDeclaration Prototype
                    | FuncDeclaration IsStatic Prototype (Maybe FunctionBody)
                    | Struct IsStatic Identifier [VarDef]
                    | Include Path
                      
                      
data Prototype = Prototype Type Identifier [VarDef]
               

data Local = LocalVar VarDef (Maybe Statement)
             

data Type = ArrType Type Statement
          | PointerType Type
          | PlainType Identifier
            

data TopStatement = ReturnStatement (Maybe Statement)
                  | Block [TopStatement]
                  | ActionStatement Statement
                  | IfStatement If [If] (Maybe [TopStatement])
                  | While If
                  | Break
                  | Continue
                    

data Statement = CallStatement Statement [Statement]
               -- The following line allows for syntax
               -- invalid trees...
               | AssignStatement Statement AssignOp Statement
               | VariableStatement Variable
               | BinaryStatement Statement Op Statement
               | NegatedStatement Statement
               | NotStatement Statement
               | PtrStatement Statement
               | ValueStatement Value
                 

data AssignOp = SetV | IncV | DecV
              | MulV | DivV | ModV
                

data Variable = LiteralVariable Identifier -- var
              | ArrayDereference Statement Statement -- stmt()[stmt()]
              | FieldDereference Statement Identifier -- stmt().field
              | FieldPtrDereference Statement Identifier -- stmt()->field
              | PtrDereference Statement -- *stmt()
                

data Value = StringValue String
           | FixedValue Double
           | IntValue Integer
           | BoolValue Bool
           | NullValue


data Op = Add | Sub | Mul | Div 
        | Lt | Lte | Eq | Nq | Gte | Gt
        | And | Or
        | BinAnd | BinOr
        | Mod

type IsConst = Bool
type IsStatic = Bool
type Identifier = String
type Path = String
type VarDef = (Type, Identifier)
type If = (Statement, [TopStatement])
type FunctionBody = ([Local], [TopStatement])