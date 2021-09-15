module Model.Definition where

import Model.Const

data Definition = Definition {
    name :: Name,
    _type :: Name,
    _default :: Expr
}

data Expr = Num Int |
            Str String |
            Bool Bool |
            Ref Name |
            Case [If] Expr |
            Func Name [Expr]

data If = If Expr Expr