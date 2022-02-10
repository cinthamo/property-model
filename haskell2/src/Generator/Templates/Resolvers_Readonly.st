    public class $p.className$ : IReadOnlyResolver
	{
        public bool IsReadOnly(IPropertyBag properties)
        {
$p.code : {s|$Resolvers_Statement(s)$}$
        }

		public string[] GetDependencies()
		{
			return new string[] { $p.used:{n|"PruebaTesis_$n$"};separator=","$ };
		}
	}

