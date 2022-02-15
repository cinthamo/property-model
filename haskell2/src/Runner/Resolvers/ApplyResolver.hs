module Runner.Resolvers.ApplyResolver where

import Model.Value
import Runner.Context
import Runner.PropertiesObject
import Runner.Resolvers.Eval
import Runner.Resolvers.Resolver

applyResolver :: PropertiesObject obj => Resolver obj
applyResolver =
  Resolver
    { beforeHas = \context ->
        if notApply context
          then GResolved False
          else GNotResolved,
      afterHas = \context hasIt -> GNotResolved,
      beforeGet = \context ->
        if (notApply context)
          then GResolved Null
          else GNotResolved,
      afterGet = \context value -> GNotResolved,
      beforeSet = \context value -> BSNotResolved,
      afterSet = \context value -> ASNotResolved,
      beforeClear = \context -> BSNotResolved,
      afterClear = \context -> ASNotResolved
    }

notApply :: PropertiesObject obj => Context obj -> Bool
notApply context = case (getApplyExpr context) of
  Just expr ->
    case (evalExpr context expr) of
      Bool False -> True
      _ -> False
  _ -> False