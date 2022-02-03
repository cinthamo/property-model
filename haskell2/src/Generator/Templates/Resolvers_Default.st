	public class $p.className$ : IDefaultResolver
	{
		public bool GetDefaultValue(IPropertyBag properties, out object value)
		{
$p.code : {s|$Resolvers_Statement(s)$}$
			return true;
		}

		public string[] GetDependencies()
		{
			return new string[] { $p.used:{n|"PruebaTesis_$n$"};separator=","$ };
		}
	}
