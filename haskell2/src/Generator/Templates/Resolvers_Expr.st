$if(e.constant)$
    $e.constant$
$elseif(e.getPropInt)$
    properties.GetPropertyValue<$e.getPropInt.pType$>("PruebaTesis_$e.getPropInt.pName$")
$elseif(e.getPropExt)$
    $e.getPropExt.target:{e|$Resolvers_Expr(e)$}$.$e.getPropExt.qName$
$elseif(e.call)$
    $e.call.fname$($e.call.param:{e|$Resolvers_Expr(e)$};separator=","$)
$elseif(e.operator)$
    $e.operator.left:{e|$Resolvers_Expr(e)$}$ $e.operator.symbol$ $e.operator.right:{e|$Resolvers_Expr(e)$}$
$elseif(e.cast)$
    ($e.cast.cType$)($e.cast.cExpr:{e|$Resolvers_Expr(e)$}$)
$endif$