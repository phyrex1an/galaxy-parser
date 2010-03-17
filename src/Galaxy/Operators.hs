module Galaxy.Operators (binaryOperators
                        ,unaryOperators
                        ,assignementOperators
                        ,lookupOp
                        ) where
import Galaxy.SyntaxTree
import Data.Maybe

binaryOperators :: [[(String, BinOp)]]
binaryOperators = [ [ ("*", Mul)
                    , ("/", Div)
                    , ("%", Mod)
                    ]
                  , [ ("+", Add)
                    , ("-", Sub)
                    ]
                  , [ ("<<", LeftShift)
                    , (">>", RightShift)
                    ]
                  , [ (">" , Greater)
                    , (">=", GreaterEqual)
                    , ("<=", LessEqual)
                    , ("<" , Less)
                    ]
                  , [ ("==", Equals)
                    , ("!=", NotEquals)
                    ]
                  , [("&",BinAnd)]
                  , [("^",BinXor)]
                  , [("|",BinOr)]
                  , [("&&",And)]
                  , [("||",Or)]
                  ]

unaryOperators :: [[(String, UnaryOp)]]
unaryOperators = [ [ ("+", UPos)
                   , ("-", UNeg)
                   , ("!", UNot)
                   , ("~", UBinNot)
                   , ("&", UAddressOf)
                   , ("*", UPtrDereference)
                   ] 
                 ]

assignementOperators :: [[(String, AssignOp)]]
assignementOperators = [ [ ("=" , SetV)
                         , ("+=", IncV)
                         , ("-=", DecV)
                         , ("*=", MulV)
                         , ("/=", DivV)
                         , ("%=", ModV)
                         , ("&=", BinAndV)
                         , ("|=", BinOrV)
                         , ("^=", BinXorV)
                         , ("~=", BinNotV)
                         , ("<<=", LeftShiftV)
                         , (">>=", RightShiftV)
                         ]
                       ]
lookupOp :: Eq a => a -> [[(String, a)]] -> String
lookupOp op ops = head . catMaybes . map (lookup op) . swapTuples $ ops 
    where
      swapTuples = map . map $ (\(a, b) -> (b, a))
