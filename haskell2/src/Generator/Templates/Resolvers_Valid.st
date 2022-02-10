    public class $p.className$ : IValidResolver
	{
        public bool IsValid(IPropertyBag propertyBag, object value)
        {
$p.code : {s|$Resolvers_Statement(s)$}$
        }

		public string[] GetDependencies()
		{
			return new string[] { $p.used:{n|"PruebaTesis_$n$"};separator=","$ };
		}
	}

