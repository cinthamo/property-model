module Model.Resolver where

import Model.Const

data Value obj = Data String |
                 Ref Name |
                 Obj obj |
                 List [Value obj] |
                 Res (Resolver obj)

data ResolveGet value = GNotResolved | GResolved value
data ResolveBeforeSet value = BSNotResolved | BSCancel | BSValue value
data ResolveAfterSet = ASNotResolved | ASResolved
data Void

data Resolver obj = Resolver
    { beforeHas   :: obj ->                      ResolveGet Bool
    , afterHas    :: obj -> Bool              -> ResolveGet Bool
    , beforeGet   :: obj ->                      ResolveGet (Value obj)
    , afterGet    :: obj -> Maybe (Value obj) -> ResolveGet (Value obj)
    , beforeSet   :: obj -> Value obj         -> ResolveBeforeSet (Value obj)
    , afterSet    :: obj -> Value obj         -> ResolveAfterSet
    , beforeClear :: obj ->                      ResolveBeforeSet Void
    , afterClear  :: obj ->                      ResolveAfterSet
    }
