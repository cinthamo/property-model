using System;
using System.Collections.Generic;
using System.Linq;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using Genexus.PropertiesLanguage.Antlr;

namespace Genexus.PropertiesLanguage
{
    public class ModelVisitor : PropParserBaseVisitor<Model>
    {
        public static readonly ModelVisitor Instance = new();

        public override Model VisitDefinitions([NotNull] PropParserParser.DefinitionsContext context)
        {
            return new Model
            {
                Definitions = context.type().Select(d => d.Accept(DefinitionListVisitor.Instance)).ToList()
            };
        }
    }

    public class DefinitionListVisitor : PropParserBaseVisitor<DefinitionList>
    {
        public static readonly DefinitionListVisitor Instance = new();

        public override DefinitionList VisitType([NotNull] PropParserParser.TypeContext context)
        {
            return new DefinitionList
            {
                Name = context.name.Text,
                ExternalType = context.ttype?.Text,
                Properties = context.property().Select(p => p.Accept(DefinitionVisitor.Instance)).ToList(),
				StartToken = context.Start,
				StopToken = context.Stop,
				OpenBracketToken = context.open
            };
        }
    }

    public class DefinitionVisitor : PropParserBaseVisitor<Definition>
    {
        public static readonly DefinitionVisitor Instance = new();

        private class AspectVisitor : PropParserBaseVisitor<IExpression?>
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

            public override IExpression? VisitRuleEqual([NotNull] PropParserParser.RuleEqualContext context)
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

            public override IExpression? VisitRuleBool([NotNull] PropParserParser.RuleBoolContext context)
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

		private class IsCollectionVisitor : PropParserBaseVisitor<Tuple<bool, IToken, IToken>?>
		{
			public static readonly IsCollectionVisitor Instance = new IsCollectionVisitor();

			private IsCollectionVisitor() { }

			private readonly string Name = "IsCollection";

			public override Tuple<bool, IToken, IToken>? VisitRuleEqual([NotNull] PropParserParser.RuleEqualContext context)
			{
				if (context.name.Text != Name)
					return null;

				if (context.ccase().Count() > 0)
					throw new Exception("IsCollection can't be conditional");

				IExpression expr = context.otherwise.Accept(ExpressionVisitor.Instance);
				if (expr is BooleanExpression)
					return Tuple.Create(expr == BooleanExpression.True, context.Start, context.Stop);
				else
					throw new Exception("IsCollection must be boolean");
			}

			public override Tuple<bool, IToken, IToken>? VisitRuleBool([NotNull] PropParserParser.RuleBoolContext context)
			{
				if (context.name.Text != Name)
					return null;

				if (context.condition != null)
					throw new Exception("IsCollection can't be conditional");

				return Tuple.Create(true, context.Start, context.Stop);
			}
		}

		private T get<T>([NotNull] PropParserParser.PropertyContext context, PropParserBaseVisitor<T> visitor, T _default)
        {
            var exprList = context.aRule()
                .Select(r => r.Accept(visitor))
                .Where(e => e != null)
                .ToList();

            if (!exprList.Any())
                return _default;

            if (exprList.Count() > 1)
                throw new Exception("Can only be one");

            return exprList.First();
        }		

		public override Definition VisitProperty([NotNull] PropParserParser.PropertyContext context)
        {
			var isCollection = get(context, IsCollectionVisitor.Instance, Tuple.Create(false, (IToken)null, (IToken)null));
			return new Definition()
			{
				Name = context.name.Text,
				Type = context.ttype.Text,
				IsCollection = isCollection.Item1,
				Description = context.ddoc?.GetText(),
				Default = get(context, AspectVisitor.Default, null),
				Apply = get(context, AspectVisitor.Apply, null),
				Readonly = get(context, AspectVisitor.Readonly, null),
				Valid = get(context, AspectVisitor.Valid, null),
				StartToken = context.Start,
				StopToken = context.Stop,
				TypeToken = context.ttype,
				OpenBracketToken = context.open,
				IsCollectionStartToken = isCollection.Item2,
				IsCollectionStopToken = isCollection.Item3
			};
        }
    }

    public class ConditionValueVisitor : PropParserBaseVisitor<ConditionValue>
    {
        public static readonly ConditionValueVisitor Instance = new();

        public override ConditionValue VisitCcase([NotNull] PropParserParser.CcaseContext context)
        {
            return new ConditionValue(context.expr(1).Accept(ExpressionVisitor.Instance),
                context.expr(0).Accept(ExpressionVisitor.Instance));
        }
    }

    public class ExpressionVisitor : PropParserBaseVisitor<IExpression>
    {
        public static readonly ExpressionVisitor Instance = new();

        public override IExpression VisitExprNumber([NotNull] PropParserParser.ExprNumberContext context)
        {
            return new NumericExpression(context, int.Parse(context.NUMBER().GetText()));
        }

        public override IExpression VisitExprBool([NotNull] PropParserParser.ExprBoolContext context)
        {
            var value = context.BOOL().GetText() == "true";
            return new BooleanExpression(context, value);
        }

        public override IExpression VisitExprString([NotNull] PropParserParser.ExprStringContext context)
        {
			var value = context.STRING().GetText();
			return new StringExpression(context, value.Substring(1, value.Length - 2)); // remove quotes
        }

        public override IExpression VisitExprNull([NotNull] PropParserParser.ExprNullContext context)
        {
            return new NullExpression(context);
        }

        public override IExpression VisitExprValue([NotNull] PropParserParser.ExprValueContext context)
        {
            return new ValueReferenceExpression(context);
        }

        public override IExpression VisitExprName([NotNull] PropParserParser.ExprNameContext context)
        {
            return new NameReferenceExpression(context, context.NAME().GetText());
        }

        public override IExpression VisitExprProp([NotNull] PropParserParser.ExprPropContext context)
        {
            return new PropertyReferenceExpression(context, context.expr().Accept(this), context.NAME().GetText());
        }

        public override IExpression VisitExprFunction([NotNull] PropParserParser.ExprFunctionContext context)
        {
            return new CallExpression(context, context.func().NAME().GetText(), context.func().expr().Select(p => p.Accept(this)).ToList());
        }

        public override IExpression VisitExprMethod([NotNull] PropParserParser.ExprMethodContext context)
        {
            return new CallExpression(context, context.func().name.Text, context.func().expr().Select(p => p.Accept(this)).Prepend(context.target.Accept(this)).ToList());
        }

        public override IExpression VisitExprOperator([NotNull] PropParserParser.ExprOperatorContext context)
        {
            return new CallExpression(context, context.op.Text, context.expr().Select(p => p.Accept(this)).ToList());
        }

        public override IExpression VisitExprNot([NotNull] PropParserParser.ExprNotContext context)
        {
            return new CallExpression(context, context.NOT().GetText(), new List<IExpression> { context.expr().Accept(this) });
        }

        public override IExpression VisitExprParenthesis([NotNull] PropParserParser.ExprParenthesisContext context)
        {
            return context.expr().Accept(this);
        }
    }
}
