using System;
using System.Collections.Generic;
using Genexus.PropertiesLanguage.Definition;

namespace Genexus.PropertiesLanguage.Conversion
{
	public class Model
	{
		public IList<string> ImportList;
		public IList<Conversion> ConversionList;
	}

	public class Conversion
	{
		public string From;
		public string To;
		public IList<Rule> RuleList;
	}

	public class Rule
    {
		public string Name;
		public IExpression Expression;
    }
}
