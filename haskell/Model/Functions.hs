module Model.Functions where

import Model.Resolver
import Model.Value

equal :: Function obj
equal [(String x), (String y)] = Bool (x == y)
equal [(Number x), (Number y)] = Bool (x == y)
equal [(Bool x), (Bool y)] = Bool (x == y)

add :: Function obj
add [(Number x), (Number y)] = Number (x + y)