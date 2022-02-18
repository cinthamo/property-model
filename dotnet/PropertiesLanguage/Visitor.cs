using Antlr4.Runtime.Misc;
using PropertiesLanguage.Antlr;

namespace PropertiesLanguage
{
    public class ModelVisitor : PropsParserBaseVisitor<Model>
    {
        public static readonly ModelVisitor Instance = new();

        public override Model VisitDefinitions([NotNull] PropsParser.DefinitionsContext context)
        {
            return new Model
            {
                Definitions = context.list().Select(d => d.Accept(DefinitionListVisitor.Instance)).ToList()
            };
        }
    }

    public class DefinitionListVisitor : PropsParserBaseVisitor<DefinitionList>
    {
        public static readonly DefinitionListVisitor Instance = new();

        public override DefinitionList VisitList([NotNull] PropsParser.ListContext context)
        {
            return new DefinitionList
            {
                Name = context.NAME(0).GetText(),
                ExternalType = context.NAME(1)?.GetText(),
                Properties = context.property().Select(p => p.Accept(DefinitionVisitor.Instance)).ToList()
            };
        }
    }

    public class DefinitionVisitor : PropsParserBaseVisitor<Definition>
    {
        public static readonly DefinitionVisitor Instance = new();

        private string GetType(IEnumerable<PropsParser.ARuleContext> typeRules)
        {
            if (!typeRules.Any())
                throw new Exception("Must have a type");

            if (typeRules.Count() > 1)
                throw new Exception("Must have only one type");

            var typeRule = typeRules.First();
            if (typeRule.RuleIndex != 0)
                throw new Exception("Type must have a value");

            if (typeRule.@case().Length > 0)
                throw new Exception("Type can not have conditions");

            IExpression typeExpr = typeRule.expr().Accept(ExpressionVisitor.Instance);
            if (typeExpr is NameReferenceExpression nameExpr)
                return nameExpr.Name;
            else
                throw new Exception("Type must be a name");
        }

        private IExpression getAspect(IEnumerable<PropsParser.ARuleContext> rules, IExpression defaultAbsent, IExpression defaultUsed, IExpression defaultOtherwise)
        {
            if (!rules.Any())
                return defaultAbsent;

            if (rules.Count() > 1)
                throw new Exception("Can only be one");

            var rule = rules.First();

            switch (rule.RuleIndex)
            {
                case 0:
                    {
                        return new CaseExpression
                        {
                            Conditions = rule.@case().Select(c => c.Accept(ConditionValueVisitor.Instance)).ToList(),
                            Otherwise = rule.expr().Accept(ExpressionVisitor.Instance)
                        };
                    }
                case 1:
                    {
                        var condition = rule.expr().Accept(ExpressionVisitor.Instance);
                        return new CaseExpression
                        {
                            Conditions = new List<ConditionValue> {
                                new ConditionValue
                                {
                                    Condition = condition,
                                    Value = defaultUsed
                                }
                            },
                            Otherwise = defaultOtherwise
                        };
                    }
                default:
                    {
                        return defaultAbsent;
                    }
            }
        }

        public override Definition VisitProperty([NotNull] PropsParser.PropertyContext context)
        {
            IEnumerable<PropsParser.ARuleContext> getRules(string name)
            {
                return context.aRule().Where(r => r.NAME().GetText() == name);
            }

            return new Definition()
            {
                Name = context.NAME().GetText(),
                Type = GetType(getRules("type")),
                Default = getAspect(getRules("default"), NullExpression.Null, NullExpression.Null, NullExpression.Null),
                Apply = getAspect(getRules("apply"), BooleanExpression.True, BooleanExpression.True, BooleanExpression.False),
                Readonly = getAspect(getRules("readonly"), BooleanExpression.False, BooleanExpression.True, BooleanExpression.False),
                Valid = getAspect(getRules("valid"), BooleanExpression.True, BooleanExpression.True, BooleanExpression.False),
            };
        }
    }

    public class ConditionValueVisitor : PropsParserBaseVisitor<ConditionValue>
    {
        public static readonly ConditionValueVisitor Instance = new();

        public override ConditionValue VisitCase([NotNull] PropsParser.CaseContext context)
        {
            return new ConditionValue
            {
                Condition = context.expr(0).Accept(ExpressionVisitor.Instance),
                Value = context.expr(1).Accept(ExpressionVisitor.Instance)
            }; 
        }
    }

    public class ExpressionVisitor : PropsParserBaseVisitor<IExpression>
    {
        public static readonly ExpressionVisitor Instance = new();

        public override IExpression VisitExpr([NotNull] PropsParser.ExprContext context)
        {
            switch (context.RuleIndex)
            {
                case 0: return new NumberExpression { Value = int.Parse(context.NUMBER().GetText()) };
                case 1: return context.BOOL().GetText() == "true" ? BooleanExpression.True : BooleanExpression.False;
                case 2: return new StringExpression { Value = context.STRING().GetText() };
                case 3: return NullExpression.Null;
                case 4: return ValueReferenceExpression.Value;
                case 5: return new NameReferenceExpression { Name = context.NAME().GetText() };
                case 6: return new PropertyReferenceExpression { Target = context.expr(0).Accept(this), Name = context.NAME().GetText() };
                case 7: return new CallExpression { Name = context.func().NAME().GetText(), Parameters = context.func().expr().Select(p => p.Accept(this)).ToList() };
                case 8: return new CallExpression { Name = context.func().NAME().GetText(), Parameters = context.func().expr().Select(p => p.Accept(this)).Prepend(context.expr(0).Accept(this)).ToList() };
                case 9: return new CallExpression { Name = context.OP().GetText(), Parameters = context.expr().Select(p => p.Accept(this)).ToList() };
                case 10: return new CallExpression { Name = context.NOT().GetText(), Parameters = context.expr().Select(p => p.Accept(this)).ToList() };
                case 11: return context.expr(0).Accept(this);
                default: return NullExpression.Null;
            }
        }
    }
}