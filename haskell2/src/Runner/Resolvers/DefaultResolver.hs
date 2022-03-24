module Runner.Resolvers.DefaultResolver where

import Data.Map as M
import Model.Definition as D
import Model.Value as V
import Runner.Const
import Runner.Context
import Runner.PropertiesObject
import Runner.Resolvers.Resolver
import Runner.Resolvers.Eval

defaultResolver :: PropertiesObject obj => Show obj => Resolver obj
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
