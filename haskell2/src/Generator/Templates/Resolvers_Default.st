	public class $p.className$ : IDefaultResolver
	{
		public bool GetDefaultValue(IPropertyBag properties, out object value)
		{
			value = 1;
			return true;
		}

		public string[] GetDependencies()
		{
			return null;
		}
	}
