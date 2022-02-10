$if(s.assign)$
			$if(s.assign.declare)$var$endif$
			$s.assign.varName$ = $s.assign.expr:{e|$Resolvers_Expr(e)$}$;
$elseif(s.aIf)$
			if ($s.aIf.condition:{e|$Resolvers_Expr(e)$}$) {
			    $s.aIf.trueBlock:{s|$Resolvers_Statement(s)$}$
$if(s.aIf.falseBlock)$
			} else {
			    $s.aIf.falseBlock:{s|$Resolvers_Statement(s)$}$
$endif$
			}
$elseif(s.aReturn)$
			return $s.aReturn:{e|$Resolvers_Expr(e)$}$;
$endif$
