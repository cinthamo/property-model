	public class $p.className$ : IApplyResolver
	{
        public bool IsApplicable(IPropertyBag properties)
        {
$p.code : {s|$Resolvers_Statement(s)$}$
        }

		public string[] GetDependencies()
		{
			return new string[] { $p.used:{n|"PruebaTesis_$n$"};separator=","$ };
		}
	}

