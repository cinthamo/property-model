using System;
using System.Collections.Generic;
using System.Linq;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using Genexus.Language.PropertiesDefinition.Antlr;

namespace Genexus.Language.PropertiesDefinition
{
    public class ModelVisitor : PDefinitionParserBaseVisitor<Model>
    {
        public static readonly ModelVisitor Instance = new();

        public override Model VisitDefinitions([NotNull] PDefinitionParserParser.DefinitionsContext context)
        {
            return new Model
            {
                ImportList = context.importt().Select(i => i.name.Text).ToList(),
                FlagsList = context.flags().Select(f => f.Accept(FlagsListVisitor.Instance)).ToList(),
                TypesList = context.type().Select(t => t.Accept(PTypeVisitor.Instance)).ToList()
            };
        }
    }

    public class FlagsListVisitor : PDefinitionParserBaseVisitor<FlagsList>
    {
        public static readonly FlagsListVisitor Instance = new();

        public override FlagsList VisitFlags([NotNull] PDefinitionParserParser.FlagsContext context)
        {
            return new FlagsList
            {
                Name = context.name.Text,
                Flags = context.flagDefinition().Select(f => f.Accept(FlagDefinitionVisitor.Instance)).ToList()
            };
        }
    }

    public class FlagDefinitionVisitor : PDefinitionParserBaseVisitor<FlagDefinition>
    {
        public static readonly FlagDefinitionVisitor Instance = new();

        public override FlagDefinition VisitFlagDefinition([NotNull] PDefinitionParserParser.FlagDefinitionContext context)
        {
            return new FlagDefinition
            {
                Name = context.name.Text,
                Type = context.ttype.Text,
                RuntimeName = context.fRule().FirstOrDefault(r => r.name.Text == "name")?.value.Text,
                Deprecated = context.fRule().FirstOrDefault(r => r.name.Text == "deprecated") != null
            };
        }
    }

    public class PTypeVisitor : PDefinitionParserBaseVisitor<PType>
    {
        public static readonly PTypeVisitor Instance = new();

        public override PType VisitType([NotNull] PDefinitionParserParser.TypeContext context)
        {
            var nameExtends = context.nameExtends().Accept(NameExtendsVisitor.Instance);
            return new PType
            {
                Name = nameExtends.Item1,
                ExtendsType = nameExtends.Item2,
                Properties = context.property().Select(p => p.Accept(PropertyDefinitionVisitor.Instance)).ToList(),
				StartToken = context.Start,
				StopToken = context.Stop,
				OpenBracketToken = context.open
            };
        }
    }

    public class NameExtendsVisitor : PDefinitionParserBaseVisitor<Tuple<string?, string?>>
    {
        public static readonly NameExtendsVisitor Instance = new();

        public override Tuple<string?, string?> VisitNameNormal([NotNull] PDefinitionParserParser.NameNormalContext context)
        {
            return Tuple.Create<string?, string?>(context.name.Text, context.ttype?.Text);
        }

        public override Tuple<string?, string?> VisitNameExternal([NotNull] PDefinitionParserParser.NameExternalContext context)
        {
            return Tuple.Create<string?, string?>(null, context.etype.Text);
        }
    }

    public class PropertyDefinitionVisitor : PDefinitionParserBaseVisitor<PropertyDefinition>
    {
        public static readonly PropertyDefinitionVisitor Instance = new();

        private class AspectVisitor : PDefinitionParserBaseVisitor<IExpression?>
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

            public override IExpression? VisitRuleEqual([NotNull] PDefinitionParserParser.RuleEqualContext context)
            {
                if (context.name.Accept(IdentifierVisitor.Instance) != Name)
                    return null;

                return new CaseExpression(context,
                    context.ccase()
                        .Select(c => c.Accept(ConditionValueVisitor.Instance))
                        .ToList(),
                    context.otherwise.Accept(ExpressionVisitor.Instance)
                );
            }

            public override IExpression? VisitRuleBool([NotNull] PDefinitionParserParser.RuleBoolContext context)
            {
                if (context.name.Accept(IdentifierVisitor.Instance) != Name)
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

        private class IdentifierVisitor : PDefinitionParserBaseVisitor<string>
        {
            public static readonly IdentifierVisitor Instance = new();

            public override string VisitIdName([NotNull] PDefinitionParserParser.IdNameContext context)
            {
                return context.name.Text;
            }

            public override string VisitIdFlag([NotNull] PDefinitionParserParser.IdFlagContext context)
            {
                return $"{context.fName.Text}.{context.name.Text}";
            }
        }

		private class IsCollectionVisitor : PDefinitionParserBaseVisitor<Tuple<bool, IToken, IToken>?>
		{
			public static readonly IsCollectionVisitor Instance = new IsCollectionVisitor();

			private IsCollectionVisitor() { }

			private readonly string Name = "IsCollection";

			public override Tuple<bool, IToken, IToken>? VisitRuleEqual([NotNull] PDefinitionParserParser.RuleEqualContext context)
			{
				if (context.name.Accept(IdentifierVisitor.Instance) != Name)
					return null;

				if (context.ccase().Count() > 0)
					throw new Exception("IsCollection can't be conditional");

				IExpression expr = context.otherwise.Accept(ExpressionVisitor.Instance);
				if (expr is BooleanExpression)
					return Tuple.Create(expr == BooleanExpression.True, context.Start, context.Stop);
				else
					throw new Exception("IsCollection must be boolean");
			}

			public override Tuple<bool, IToken, IToken>? VisitRuleBool([NotNull] PDefinitionParserParser.RuleBoolContext context)
			{
				if (context.name.Accept(IdentifierVisitor.Instance) != Name)
					return null;

				if (context.condition != null)
					throw new Exception("IsCollection can't be conditional");

				return Tuple.Create(true, context.Start, context.Stop);
			}
		}

		private T get<T>([NotNull] PDefinitionParserParser.PropertyContext context, PDefinitionParserBaseVisitor<T> visitor, T _default)
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

		public override PropertyDefinition VisitProperty([NotNull] PDefinitionParserParser.PropertyContext context)
        {
			var isCollection = get(context, IsCollectionVisitor.Instance, Tuple.Create(false, (IToken)null, (IToken)null));
			return new PropertyDefinition()
			{
				Name = context.name.GetText(),
				Type = context.ttype.Text,
				IsCollection = isCollection.Item1,
				Description = context.ddoc?.Accept(DescriptionVisitor.Instance),
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

	public class DescriptionVisitor : PDefinitionParserBaseVisitor<string>
	{
		public static readonly DescriptionVisitor Instance = new();

		public override string VisitDoc([NotNull] PDefinitionParserParser.DocContext context)
		{
			if (context.EOL_DOC() != null)
				return context.EOL_DOC().GetText().Substring(3).TrimStart();

			if (context.BLOCK_DOC() != null)
			{
				var text = context.BLOCK_DOC().GetText();
				text = text.Substring(3, text.Length - 5);
				int startIndex = text.IndexOf("\n");
				if (startIndex != -1)
				{
					int endIndex = startIndex + 1;
					while (text[endIndex] == ' ' || text[endIndex] == '\t')
						endIndex++;
					var space = text.Substring(startIndex, endIndex - startIndex);
					text = text.Replace(space, "\n").Trim();
				}
				return text;
			}

			throw new Exception("Invalid documentation");
		}
	}

    public class ConditionValueVisitor : PDefinitionParserBaseVisitor<ConditionValue>
    {
        public static readonly ConditionValueVisitor Instance = new();

        public override ConditionValue VisitCcase([NotNull] PDefinitionParserParser.CcaseContext context)
        {
            return new ConditionValue(context.expr(1).Accept(ExpressionVisitor.Instance),
                context.expr(0).Accept(ExpressionVisitor.Instance));
        }
    }

    public class ExpressionVisitor : PDefinitionParserBaseVisitor<IExpression>
    {
        public static readonly ExpressionVisitor Instance = new();

        public override IExpression VisitExprNumber([NotNull] PDefinitionParserParser.ExprNumberContext context)
        {
            return new NumericExpression(context, int.Parse(context.NUMBER().GetText()));
        }

        public override IExpression VisitExprBool([NotNull] PDefinitionParserParser.ExprBoolContext context)
        {
            var value = context.BOOL().GetText() == "true";
            return new BooleanExpression(context, value);
        }

        public override IExpression VisitExprString([NotNull] PDefinitionParserParser.ExprStringContext context)
        {
			var value = context.STRING_DOUBLE().GetText();
			return new StringExpression(context, value.Substring(1, value.Length - 2)); // remove quotes
        }

        public override IExpression VisitExprNull([NotNull] PDefinitionParserParser.ExprNullContext context)
        {
            return new NullExpression(context);
        }

        public override IExpression VisitExprValue([NotNull] PDefinitionParserParser.ExprValueContext context)
        {
            return new ValueReferenceExpression(context);
        }

        public override IExpression VisitExprName([NotNull] PDefinitionParserParser.ExprNameContext context)
        {
            return new NameReferenceExpression(context, context.name.GetText());
        }

        public override IExpression VisitExprProp([NotNull] PDefinitionParserParser.ExprPropContext context)
        {
            return new PropertyReferenceExpression(context, context.expr().Accept(this), context.NAME().GetText());
        }

        public override IExpression VisitExprFunction([NotNull] PDefinitionParserParser.ExprFunctionContext context)
        {
            return new CallExpression(context, context.func().NAME().GetText(), context.func().expr().Select(p => p.Accept(this)).ToList());
        }

        public override IExpression VisitExprMethod([NotNull] PDefinitionParserParser.ExprMethodContext context)
        {
            return new CallExpression(context, context.func().name.Text, context.func().expr().Select(p => p.Accept(this)).Prepend(context.target.Accept(this)).ToList());
        }

        public override IExpression VisitExprOperator([NotNull] PDefinitionParserParser.ExprOperatorContext context)
        {
            return new CallExpression(context, context.op.Text, context.expr().Select(p => p.Accept(this)).ToList());
        }

        public override IExpression VisitExprNot([NotNull] PDefinitionParserParser.ExprNotContext context)
        {
            return new CallExpression(context, context.NOT().GetText(), new List<IExpression> { context.expr().Accept(this) });
        }

        public override IExpression VisitExprParenthesis([NotNull] PDefinitionParserParser.ExprParenthesisContext context)
        {
            return context.expr().Accept(this);
        }
    }
}
