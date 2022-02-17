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
        public IExpression? Default;
        public IExpression? Apply;
        public IExpression? Readonly;
        public IExpression? Valid;
    }

    public interface IExpression { }

    public struct StringExpression : IExpression
    {
        public string Value;
    }

    public struct NumberExpression : IExpression
    {
        public int Value;
    }

    public struct BooleanExpression : IExpression
    {
        public bool Value;

        public static readonly BooleanExpression True = new() { Value = true };
        public static readonly BooleanExpression False = new() { Value = false };
    }

    public struct NullExpression : IExpression {
        public static readonly NullExpression Null = new();
    }

    public struct ValueReferenceExpression : IExpression {
        public static readonly ValueReferenceExpression Value = new();
    }

    public struct NameReferenceExpression : IExpression
    {
        public string Name;
    }

    public struct PropertyReferenceExpression : IExpression
    {
        public IExpression Target;
        public string Name;
    }

    public struct CaseExpression : IExpression
    {
        public List<ConditionValue> Conditions;
        public IExpression Otherwise;
    }

    public struct ConditionValue
    {
        public IExpression Condition;
        public IExpression Value;
    }

    public struct CallExpression : IExpression
    {
        public string Name;
        public List<IExpression> Parameters;
        // methodName object++parameters, External Object Method Call
        // procedureName parameters, GX Procedure Call
        // functionName parameters, language supported functions
    }
}
