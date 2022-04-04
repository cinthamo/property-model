using System;
using System.Collections.Generic;
using Genexus.PropertiesLanguage.Definition;

namespace Genexus.PropertiesLanguage.Transformation
{
	public class Model
	{
		public IList<string> ImportList;
		public IList<Conversion> ConversionList;
		public IList<Rewrite> RewriteList;
		public IList<Transform> TransformList;
	}

	public class BaseTransform
    {
		public string Name;
		public string From;
		public string To;
	}

	public class Conversion : BaseTransform
	{
		public IList<CRule> RuleList;
	}

	public class CRule
	{
		public string Name;
		public IExpression Expression;
	}

	public class Rewrite : BaseTransform
	{
		public IList<RRule> RuleList;
	}

	public class RRule
    {
		public string From;
		public string To;
    }

	public class Transform : BaseTransform
    {
		public IList<string> TransformList;
    }
}
