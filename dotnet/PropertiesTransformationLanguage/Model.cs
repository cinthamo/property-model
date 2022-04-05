using System;
using System.Collections.Generic;
using Antlr4.Runtime;
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
		public string? Name;
		public TypeName From;
		public TypeName To;
	}

	public class TypeName
    {
		public string? PropertiesDefinition;
		public string Type;
    }

	public class Conversion : BaseTransform
	{
		public ObjectExpression Object;
	}

	public class CRule
	{
		public string Name;
		public IExpression Value;
		public IExpression? Condition;
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

	public class ObjectExpression : BaseExpression
    {
		public ObjectExpression(ParserRuleContext? context): base(context) { }
		public IList<CRule> RuleList;
	}

	public class ListExpression : BaseExpression
    {
		public ListExpression(ParserRuleContext? context): base(context) { }
		public IList<IExpression> Values;
    }
}
