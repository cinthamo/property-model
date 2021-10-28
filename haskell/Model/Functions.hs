module Model.Functions where

import Model.Value
import Model.Resolvers.Resolver

equal :: Function obj
equal [(String x), (String y)] = Bool (x == y)
equal [(Number x), (Number y)] = Bool (x == y)
equal [(Bool x), (Bool y)] = Bool (x == y)

add :: Function obj
add [(Number x), (Number y)] = Number (x + y)

greater :: Function obj
greater [(Number x), (Number y)] = Bool (x > y)
