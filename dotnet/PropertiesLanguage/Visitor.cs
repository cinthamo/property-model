using System;
using System.Collections.Generic;
using System.Linq;
using Antlr4.Runtime.Misc;
using Genexus.PropertiesLanguage.Antlr;

namespace Genexus.PropertiesLanguage
{
    public class ModelVisitor : PGrammarBaseVisitor<Model>
    {
        public static readonly ModelVisitor Instance = new();

        public override Model VisitDefinitions([NotNull] PGrammar.DefinitionsContext context)
        {
            return new Model
            {
                Definitions = context.type().Select(d => d.Accept(DefinitionListVisitor.Instance)).ToList()
            };
        }
    }

    public class DefinitionListVisitor : PGrammarBaseVisitor<DefinitionList>
    {
        public static readonly DefinitionListVisitor Instance = new();

        public override DefinitionList VisitType([NotNull] PGrammar.TypeContext context)
        {
            return new DefinitionList
            {
                Name = context.name.Text,
                ExternalType = context.ttype?.Text,
                Properties = context.property().Select(p => p.Accept(DefinitionVisitor.Instance)).ToList()
            };
        }
    }

    public class DefinitionVisitor : PGrammarBaseVisitor<Definition>
    {
        public static readonly DefinitionVisitor Instance = new();

        private class AspectVisitor : PGrammarBaseVisitor<IExpression?>
        {
            public static readonly AspectVisitor Default = new AspectVisitor("default", NullExpression.Null, NullExpression.Null);
            public static readonly AspectVisitor Apply = new AspectVisitor("apply", BooleanExpression.True, BooleanExpression.False);
            public static readonly AspectVisitor Readonly = new AspectVisitor("readonly", BooleanExpression.True, BooleanExpression.False);
            public static readonly AspectVisitor Valid = new AspectVisitor("valid", BooleanExpression.True, BooleanExpression.False);

            private AspectVisitor(string name, IExpression defaultUsed, IExpression defaultOtherwise)
            {
                Name = name;
                Used = defaultUsed;
                Otherwise = defaultOtherwise;
            }

            private readonly string Name;
            private readonly IExpression Used;
            private readonly IExpression Otherwise;

            public override IExpression? VisitRuleEqual([NotNull] PGrammar.RuleEqualContext context)
            {
                if (context.name.Text != Name)
                    return null;

                return new CaseExpression(context,
                    context.ccase()
                        .Select(c => c.Accept(ConditionValueVisitor.Instance))
                        .ToList(),
                    context.otherwise.Accept(ExpressionVisitor.Instance)
                );
            }

            public override IExpression? VisitRuleBool([NotNull] PGrammar.RuleBoolContext context)
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

        private IExpression? getAspect([NotNull] PGrammar.PropertyContext context, AspectVisitor visitor)
        {
            var exprList = context.aRule()
                .Select(r => r.Accept(visitor))
                .Where(e => e != null)
                .ToList();

            if (!exprList.Any())
                return null;

            if (exprList.Count() > 1)
                throw new Exception("Can only be one");

            return exprList.First();
        }

        public override Definition VisitProperty([NotNull] PGrammar.PropertyContext context)
        {
            return new Definition()
            {
                Name = context.name.Text,
                Type = context.ttype.Text,
                Description = context.ddoc?.GetText(),
                Default = getAspect(context, AspectVisitor.Default),
                Apply = getAspect(context, AspectVisitor.Apply),
                Readonly = getAspect(context, AspectVisitor.Readonly),
                Valid = getAspect(context, AspectVisitor.Valid),
            };
        }
    }

    public class ConditionValueVisitor : PGrammarBaseVisitor<ConditionValue>
    {
        public static readonly ConditionValueVisitor Instance = new();

        public override ConditionValue VisitCcase([NotNull] PGrammar.CcaseContext context)
        {
            return new ConditionValue(context.expr(1).Accept(ExpressionVisitor.Instance),
                context.expr(0).Accept(ExpressionVisitor.Instance));
        }
    }

    public class ExpressionVisitor : PGrammarBaseVisitor<IExpression>
    {
        public static readonly ExpressionVisitor Instance = new();

        public override IExpression VisitExprNumber([NotNull] PGrammar.ExprNumberContext context)
        {
            return new NumericExpression(context, int.Parse(context.NUMBER().GetText()));
        }

        public override IExpression VisitExprBool([NotNull] PGrammar.ExprBoolContext context)
        {
            var value = context.BOOL().GetText() == "true";
            return new BooleanExpression(context, value);
        }

        public override IExpression VisitExprString([NotNull] PGrammar.ExprStringContext context)
        {
			var value = context.STRING().GetText();
			return new StringExpression(context, value.Substring(1, value.Length - 2)); // remove quotes
        }

        public override IExpression VisitExprNull([NotNull] PGrammar.ExprNullContext context)
        {
            return new NullExpression(context);
        }

        public override IExpression VisitExprValue([NotNull] PGrammar.ExprValueContext context)
        {
            return new ValueReferenceExpression(context);
        }

        public override IExpression VisitExprName([NotNull] PGrammar.ExprNameContext context)
        {
            return new NameReferenceExpression(context, context.NAME().GetText());
        }

        public override IExpression VisitExprProp([NotNull] PGrammar.ExprPropContext context)
        {
            return new PropertyReferenceExpression(context, context.expr().Accept(this), context.NAME().GetText());
        }

        public override IExpression VisitExprFunction([NotNull] PGrammar.ExprFunctionContext context)
        {
            return new CallExpression(context, context.func().NAME().GetText(), context.func().expr().Select(p => p.Accept(this)).ToList());
        }

        public override IExpression VisitExprMethod([NotNull] PGrammar.ExprMethodContext context)
        {
            return new CallExpression(context, context.func().name.Text, context.func().expr().Select(p => p.Accept(this)).Prepend(context.target.Accept(this)).ToList());
        }

        public override IExpression VisitExprOperator([NotNull] PGrammar.ExprOperatorContext context)
        {
            return new CallExpression(context, context.op.Text, context.expr().Select(p => p.Accept(this)).ToList());
        }

        public override IExpression VisitExprNot([NotNull] PGrammar.ExprNotContext context)
        {
            return new CallExpression(context, context.NOT().GetText(), new List<IExpression> { context.expr().Accept(this) });
        }

        public override IExpression VisitExprParenthesis([NotNull] PGrammar.ExprParenthesisContext context)
        {
            return context.expr().Accept(this);
        }
    }
}
