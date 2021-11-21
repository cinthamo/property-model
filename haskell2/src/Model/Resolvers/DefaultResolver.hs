module Model.Resolvers.DefaultResolver where

import Model.PropertiesObject
import Model.Const
import Model.Context
import Model.Definition as D
import Model.Value as V
import Model.Resolvers.Resolver
import Model.Resolvers.Eval
import Data.Map as M

defaultResolver :: PropertiesObject obj => Resolver obj
defaultResolver = Resolver {
    beforeHas   = \context       -> GNotResolved,
    afterHas    = \context hasIt ->
        if hasIt then
            GNotResolved
        else case (getDefaultExpr context) of
            Just _ -> GResolved True
            _ -> GNotResolved,

    beforeGet   = \context       -> GNotResolved,
    afterGet    = \context value ->
        case value of
            Just v -> GNotResolved
            _ -> case (getDefaultExpr context) of
                Just expr -> GResolved (evalExpr context expr)
                _ -> GNotResolved,

    beforeSet   = \context value -> BSNotResolved,
    afterSet    = \context value -> ASNotResolved,
    beforeClear = \context       -> BSNotResolved,
    afterClear  = \context       -> ASNotResolved
}
