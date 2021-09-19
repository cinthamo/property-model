module Model.Definition where

import Model.Const

data ObjectDefinition =
    ObjectDefinition {
      properties :: [Definition],
      related :: [Name]
    }

data Definition =
    Definition {
        name :: Name,
        _default :: Expr
    } |
    External {
        name :: Name
    }

data Expr = Num Int |
            Str String |
            Bool Bool |
            Ref Name |
            RefX Name Name |
            Case [If] Expr |
            Func Name [Expr]

data If = If Expr Expr