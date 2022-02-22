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
                Name = context.name.Text,
                ExternalType = context.type?.Text,
                Properties = context.property().Select(p => p.Accept(DefinitionVisitor.Instance)).ToList()
            };
        }
    }

    public class DefinitionVisitor : PropsParserBaseVisitor<Definition>
    {
        public static readonly DefinitionVisitor Instance = new();

        private class TypeVisitor : PropsParserBaseVisitor<string>
        {
            public static readonly TypeVisitor Instance = new();

            public override string VisitRuleEqual([NotNull] PropsParser.RuleEqualContext context)
            {
                if (context.name.Text != "type")
                    return String.Empty;

                if (context.@case().Any())
                    throw new Exception("Type can not have conditions");

                if (context.otherwise is PropsParser.ExprNameContext nameExpr)
                    return nameExpr.name.Text;
                else
                    throw new Exception("Type must be a name");
            }
        }

        private static string GetType([NotNull] PropsParser.PropertyContext context)
        {
            var exprList = context.aRule()
                .Select(r => r.Accept(TypeVisitor.Instance))
                .Where(e => !string.IsNullOrEmpty(e))
                .ToList();

            if (!exprList.Any())
                throw new Exception("Must have a type");

            if (exprList.Count() > 1)
                throw new Exception("Must have only one type");

            return exprList.First();
        }

        private class AspectVisitor : PropsParserBaseVisitor<IExpression?>
        {
            public static readonly AspectVisitor Default = new AspectVisitor("default", NullExpression.Null, NullExpression.Null, NullExpression.Null);
            public static readonly AspectVisitor Apply = new AspectVisitor("apply", BooleanExpression.True, BooleanExpression.True, BooleanExpression.False);
            public static readonly AspectVisitor Readonly = new AspectVisitor("readonly", BooleanExpression.False, BooleanExpression.True, BooleanExpression.False);
            public static readonly AspectVisitor Valid = new AspectVisitor("valid", BooleanExpression.True, BooleanExpression.True, BooleanExpression.False);

            private AspectVisitor(string name, IExpression defaultAbsent, IExpression defaultUsed, IExpression defaultOtherwise)
            {
                Name = name;
                Absent = defaultAbsent;
                Used = defaultUsed;
                Otherwise = defaultOtherwise;
            }

            private readonly string Name;
            public readonly IExpression Absent;
            private readonly IExpression Used;
            private readonly IExpression Otherwise;

            public override IExpression? VisitRuleEqual([NotNull] PropsParser.RuleEqualContext context)
            {
                if (context.name.Text != Name)
                    return null;

                return new CaseExpression(context,
                    context.@case()
                        .Select(c => c.Accept(ConditionValueVisitor.Instance))
                        .ToList(),
                    context.otherwise.Accept(ExpressionVisitor.Instance)
                );
            }

            public override IExpression? VisitRuleBool([NotNull] PropsParser.RuleBoolContext context)
            {
                if (context.name.Text != Name)
                    return null;

                if (context.condition == null)
                    return Used;

                return new CaseExpression(context,
                    new List<ConditionValue> {
                            new ConditionValue(context.condition.Accept(ExpressionVisitor.Instance), Used)
                    },
                    Otherwise
                );
            }
        }

        private IExpression getAspect([NotNull] PropsParser.PropertyContext context, AspectVisitor visitor)
        {
            var exprList = context.aRule()
                .Select(r => r.Accept(visitor))
                .Where(e => e != null)
                .ToList();

            if (!exprList.Any())
                return visitor.Absent;

            if (exprList.Count() > 1)
                throw new Exception("Can only be one");

            return exprList.First();
        }

        public override Definition VisitProperty([NotNull] PropsParser.PropertyContext context)
        {
            return new Definition()
            {
                Name = context.NAME().GetText(),
                Type = GetType(context),
                Default = getAspect(context, AspectVisitor.Default),
                Apply = getAspect(context, AspectVisitor.Apply),
                Readonly = getAspect(context, AspectVisitor.Readonly),
                Valid = getAspect(context, AspectVisitor.Valid),
            };
        }
    }

    public class ConditionValueVisitor : PropsParserBaseVisitor<ConditionValue>
    {
        public static readonly ConditionValueVisitor Instance = new();

        public override ConditionValue VisitCase([NotNull] PropsParser.CaseContext context)
        {
            return new ConditionValue(context.expr(0).Accept(ExpressionVisitor.Instance),
                context.expr(1).Accept(ExpressionVisitor.Instance));
        }
    }

    public class ExpressionVisitor : PropsParserBaseVisitor<IExpression>
    {
        public static readonly ExpressionVisitor Instance = new();

        public override IExpression VisitExprNumber([NotNull] PropsParser.ExprNumberContext context)
        {
            return new NumberExpression(context, int.Parse(context.NUMBER().GetText()));
        }

        public override IExpression VisitExprBool([NotNull] PropsParser.ExprBoolContext context)
        {
            var value = context.BOOL().GetText() == "true";
            return new BooleanExpression(context, value);
        }

        public override IExpression VisitExprString([NotNull] PropsParser.ExprStringContext context)
        {
            return new StringExpression(context, context.STRING().GetText());
        }

        public override IExpression VisitExprNull([NotNull] PropsParser.ExprNullContext context)
        {
            return new NullExpression(context);
        }

        public override IExpression VisitExprValue([NotNull] PropsParser.ExprValueContext context)
        {
            return new ValueReferenceExpression(context);
        }

        public override IExpression VisitExprName([NotNull] PropsParser.ExprNameContext context)
        {
            return new NameReferenceExpression(context, context.NAME().GetText());
        }

        public override IExpression VisitExprProp([NotNull] PropsParser.ExprPropContext context)
        {
            return new PropertyReferenceExpression(context, context.expr().Accept(this), context.NAME().GetText());
        }

        public override IExpression VisitExprFunction([NotNull] PropsParser.ExprFunctionContext context)
        {
            return new CallExpression(context, context.func().NAME().GetText(), context.func().expr().Select(p => p.Accept(this)).ToList());
        }

        public override IExpression VisitExprMethod([NotNull] PropsParser.ExprMethodContext context)
        {
            return new CallExpression(context, context.func().name.Text, context.func().expr().Select(p => p.Accept(this)).Prepend(context.target.Accept(this)).ToList());
        }

        public override IExpression VisitExprOperator([NotNull] PropsParser.ExprOperatorContext context)
        {
            return new CallExpression(context, context.OP().GetText(), context.expr().Select(p => p.Accept(this)).ToList());
        }

        public override IExpression VisitExprNot([NotNull] PropsParser.ExprNotContext context)
        {
            return new CallExpression(context, context.NOT().GetText(), new List<IExpression> { context.expr().Accept(this) });
        }

        public override IExpression VisitExprParenthesis([NotNull] PropsParser.ExprParenthesisContext context)
        {
            return context.expr().Accept(this);
        }
    }
}
