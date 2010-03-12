{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Galaxy.Show where

import Text.PrettyPrint
import Galaxy.SyntaxTree


class Pretty a where
    doc :: a -> Doc

instance (Pretty a) => Pretty (Maybe a) where
    doc (Just a) = doc a
    doc Nothing = empty

instance Pretty File where
    doc (File _ td) = foldl ($+$) empty (map doc td)

instance Pretty TopDeclaration where
    doc (ConstDeclaration c v s) =
        static c <+> varDef v <+> equals <+> doc s <> semi
    doc (VariableDeclaration c v s) = 
        static c <+> varDef v <+> perhaps s (\s -> equals <+> doc s) <> semi
    doc (TypeDef t i) = text "typedef" <+> doc t <+> text i
    doc (NativeDeclaration p) =
        text "native" <+> doc p
    doc (FuncDeclaration c p b) =
        static c <+> doc p $+$ perhaps b (\b -> funcBody b)
    doc (Struct c i v) = 
        static c <+> text "struct" <+> text i <+> (braces
                . nest 4 $ (hsep $ punctuate semi (map varDef v)))
    doc (Include p) = text "include" <+> text (show p) <> semi

static c = if c then text "static" else empty

varDef (t, i) = doc t <+> text i

perhaps Nothing _ = empty
perhaps (Just v) f = f v

instance Pretty Prototype where
    doc (Prototype t i vs) = doc t <+> text i <> parens (hsep $ punctuate comma (map varDef vs))

instance Pretty Local where
    doc (LocalVar v s) = varDef v <+> perhaps s (\s -> equals <+> doc s) <> semi

instance Pretty Type where
    doc (ArrType t s) = doc t <+> brackets (doc s)
    doc (PointerType t) = doc t <+> char '*'
    doc (PlainType i) = text i

instance Pretty TopStatement where
    doc (ReturnStatement s) = text "return" <+> doc s <> semi
    doc (ActionStatement s) = doc s <> semi
    doc (IfStatement i ifs els) = (hsep $ punctuate (text "else") ((ifBody i):(map ifBody ifs)))
                                  $+$ case els of
                                        Nothing -> empty
                                        Just stmts -> text "else" <> funcBody ([],stmts)
                                      where
                                        ifBody = condBody "if"
    doc (While i) = condBody "while" i
    doc Break = text "break" <> semi
    doc Continue = text "continue" <> semi

condBody n (s, body) = text n <+> parens (doc s) 
                   $+$ funcBody ([], body)

funcBody :: FunctionBody -> Doc
funcBody (v,b) = lbrace
                $+$ nest 4 (
                            (vcat $ map doc v) <+> 
                            (vcat $ map doc b)
                           )
                $+$ rbrace

instance Pretty [Statement] where
    doc stmts = sep $ punctuate (char ',') (map doc stmts)

instance Pretty Statement where
    doc (CallStatement s ss) = doc s <> parens (hsep $ punctuate comma (map doc ss))
    doc (AssignStatement v o s) = doc v <> doc o <> doc s
    doc (VariableStatement i) = doc i
    doc (BinaryStatement b1 o b2) = doc b1 <+> doc o <+> doc b2
    doc (NegatedStatement s) = char '-' <> doc s
    doc (NotStatement s) = char '!' <> doc s
    doc (PtrStatement s) = char '&' <> doc s
    doc (ValueStatement v) = doc v

instance Pretty AssignOp where
    doc SetV = text "="
    doc IncV = text "+="
    doc DecV = text "-="
    doc MulV = text "*="
    doc DivV = text "/="
    doc ModV = text "%="

instance Pretty Variable where
    doc (LiteralVariable i) = text i
    doc (ArrayDereference s i) = doc s <> brackets (doc s)
    doc (FieldPtrDereference s i) = doc s <> text "->" <> text i
    doc (FieldDereference s i) = doc s <> char '.' <> text i
    doc (PtrDereference s) = char '*' <> doc s

instance Pretty Value where
    doc (StringValue s) = text $ show s
    doc (FixedValue r) = double r
    doc (IntValue i) = integer i
    doc (BoolValue b) = if b then text "true" else text "false"
    doc NullValue = text "null"

instance Pretty Op where
    doc Add = char '+'
    doc Sub = char '-'
    doc Mul = char '*'
    doc Div = char '/'
    doc Lt  = char '<'
    doc Lte = text "<="
    doc Eq  = text "=="
    doc Nq  = text "!="
    doc Gte = text ">="
    doc Gt  = char '>'
    doc And = text "&&"
    doc Or  = text "||"
    doc BinAnd = char '&'
    doc BinOr  = char '|'
    doc Mod = char '%'

renderDoc :: Pretty a => a -> String
renderDoc = render . doc