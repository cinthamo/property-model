using System.Collections.Generic;
using Antlr4.Runtime;

namespace Genexus.PropertiesLanguage
{
    public partial class Model
    {
        public IList<DefinitionList> Definitions;
	}

    public partial class DefinitionList
    {
        public string? Name;
        public string? ExtendsType;
        public IList<Definition> Properties;
		public bool IsNew;
		public IToken StartToken;
		public IToken StopToken;
		public IToken OpenBracketToken;
	}

	public partial class Definition
    {
        public string Name;
        public string Type;
		public bool IsCollection;
		public string Description;
		public IExpression Default;
        public IExpression Apply;
        public IExpression Readonly;
        public IExpression Valid;
		public bool IsNew;
		public bool IsTypeChanged;
		public bool IsCollectionChanged;
		public IToken StartToken;
		public IToken StopToken;
		public IToken TypeToken;
		public IToken OpenBracketToken;
		public IToken IsCollectionStartToken;
		public IToken IsCollectionStopToken;
	}

	public interface IExpression
	{
        string Text { get; }
        string Position { get; }
    }

    public class BaseExpression : IExpression
    {
        public BaseExpression(ParserRuleContext? context)
        {
            if (context == null)
            {
                Text = "Expression";
                Position = "unknown position";
            }
            else
            {
                Text = context.GetText();
                Position = $"Line {context.Start.Line} Char {context.Start.Column}";
            }
        }

        public string Text { get; private set; }
        public string Position { get; private set; }

        public override string ToString()
        {
            return $"{Text} ({Position})";
        }
    }

    public class StringExpression : BaseExpression
    {
        public StringExpression(ParserRuleContext context, string value): base(context)
        {
            Value = value;
        }

        public string Value { get; private set; }
    }

    public class NumericExpression : BaseExpression
    {
        public NumericExpression(ParserRuleContext context, int value) : base(context)
        {
            Value = value;
        }

        public int Value { get; private set; }
    }

    public class BooleanExpression : BaseExpression
    {
        public BooleanExpression(ParserRuleContext? context, bool value) : base(context)
        {
            Value = value;
        }

        public bool Value { get; private set; }

        public static readonly IExpression True = new BooleanExpression(null, true);
        public static readonly IExpression False = new BooleanExpression(null, false);

		public override string ToString()
		{
			return Value.ToString();
		}
	}

    public class NullExpression : BaseExpression
    {
        public NullExpression(ParserRuleContext? context) : base(context) { }

        public static readonly IExpression Null = new NullExpression(null);
    }

    public class ValueReferenceExpression : BaseExpression
    {
        public ValueReferenceExpression(ParserRuleContext context) : base(context) { }
    }

    public class NameReferenceExpression : BaseExpression
    {
        public NameReferenceExpression(ParserRuleContext context, string name) : base(context)
        {
            Name = name;
        }

        public string Name { get; private set; }
    }

    public class PropertyReferenceExpression : BaseExpression
    {
        public PropertyReferenceExpression(ParserRuleContext context, IExpression target, string name) : base(context)
        {
            Target = target;
            Name = name;
        }

        public IExpression Target { get; private set; }
        public string Name { get; private set; }
    }

    public class CaseExpression : BaseExpression
    {
        public CaseExpression(ParserRuleContext context, List<ConditionValue> conditions, IExpression otherwise) : base(context)
        {
            Conditions = conditions;
            Otherwise = otherwise;
        }

        public List<ConditionValue> Conditions { get; private set; }
        public IExpression Otherwise { get; private set; }
    }

    public class ConditionValue
    {
        public ConditionValue(IExpression condition, IExpression value)
        {
            Condition = condition;
            Value = value;
        }

        public IExpression Condition { get; private set; }
        public IExpression Value { get; private set; }
    }

    public class CallExpression : BaseExpression
    {
        public CallExpression(ParserRuleContext context, string name, List<IExpression> parameters) : base(context)
        {
            Name = name;
            Parameters = parameters;
        }

        public string Name { get; private set; }
        public List<IExpression> Parameters { get; private set; }
        // methodName object++parameters, External Object Method Call
        // procedureName parameters, GX Procedure Call
        // functionName parameters, language supported functions
    }
}
