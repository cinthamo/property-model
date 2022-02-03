$if(e.constant)$
    $e.constant$
$elseif(e.getProp)$
    properties.GetPropertyValue<$e.getProp.pType$>("PruebaTesis_$e.getProp.pName$")
$elseif(e.call)$
    $e.call.fname$($e.call.param:{e|$Resolvers_Expr(e)$};separator=","$)
$elseif(e.operator)$
    $e.operator.left:{e|$Resolvers_Expr(e)$}$ $e.operator.symbol$ $e.operator.right:{e|$Resolvers_Expr(e)$}$
$endif$