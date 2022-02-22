using Antlr4.Runtime;

namespace PropertiesLanguage
{
    public struct Model
    {
        public List<DefinitionList> Definitions;
    }

    public struct DefinitionList
    {
        public string Name;
        public string? ExternalType;
        public List<Definition> Properties;
    }

    public struct Definition
    {
        public string Name;
        public string Type;
        public IExpression Default;
        public IExpression Apply;
        public IExpression Readonly;
        public IExpression Valid;
    }

    public interface IExpression { }

    public class BaseExpression : IExpression
    {
        public BaseExpression(ParserRuleContext? context)
        {
            if (context == null)
                position = null;
            else
                position = $"{context.GetText()} (Line {context.Start.Line} Char {context.Start.Column}";
        }

        private readonly string? position;

        public override string ToString()
        {
            return position ?? base.ToString() ?? "Expression";
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

    public class NumberExpression : BaseExpression
    {
        public NumberExpression(ParserRuleContext context, int value) : base(context)
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
