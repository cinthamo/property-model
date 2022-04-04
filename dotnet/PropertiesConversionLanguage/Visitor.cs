using System;
using System.Collections.Generic;
using System.Linq;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using Genexus.PropertiesLanguage.Definition;
using Genexus.PropertiesLanguage.Conversion.Antlr;

namespace Genexus.PropertiesLanguage.Conversion
{
    public class ModelVisitor : PConversionParserBaseVisitor<Model>
    {
        public static readonly ModelVisitor Instance = new();

        public override Model VisitConversions([NotNull] PConversionParserParser.ConversionsContext context)
        {
            return new Model
            {
                ImportList = context.importt().Select(i => i.name.Text).ToList(),
                ConversionList = context.conversion().Select(f => f.Accept(ConversionVisitor.Instance)).ToList()
            };
        }
    }

    public class ConversionVisitor : PConversionParserBaseVisitor<Conversion>
    {
        public static readonly ConversionVisitor Instance = new();

        public override Conversion VisitConversion([NotNull] PConversionParserParser.ConversionContext context)
        {
            return new Conversion
            {
                From = context.from.Text,
                To = context.to.Text,
                RuleList = context.crule().Select(r => r.Accept(RuleVisitor.Instance)).ToList()
            };
        }
    }

    public class RuleVisitor : PConversionParserBaseVisitor<Rule>
    {
        public static readonly RuleVisitor Instance = new();

        public override Rule VisitCrule([NotNull] PConversionParserParser.CruleContext context)
        {
            return new Rule
            {
                Name = context.name.Text,
                Expression = context.expr().Accept(ExpressionVisitor.Instance)
            };
        }
    }

    // Copied from PropertiesDefinitionLanguage/Visitor.cs
    public class ExpressionVisitor : PConversionParserBaseVisitor<IExpression>
    {
        public static readonly ExpressionVisitor Instance = new();

        public override IExpression VisitExprNumber([NotNull] PConversionParserParser.ExprNumberContext context)
        {
            return new NumericExpression(context, int.Parse(context.NUMBER().GetText()));
        }

        public override IExpression VisitExprBool([NotNull] PConversionParserParser.ExprBoolContext context)
        {
            var value = context.BOOL().GetText() == "true";
            return new BooleanExpression(context, value);
        }

        public override IExpression VisitExprString([NotNull] PConversionParserParser.ExprStringContext context)
        {
            var value = context.STRING_DOUBLE().GetText();
            return new StringExpression(context, value.Substring(1, value.Length - 2)); // remove quotes
        }

        public override IExpression VisitExprNull([NotNull] PConversionParserParser.ExprNullContext context)
        {
            return new NullExpression(context);
        }

        public override IExpression VisitExprValue([NotNull] PConversionParserParser.ExprValueContext context)
        {
            return new ValueReferenceExpression(context);
        }

        public override IExpression VisitExprName([NotNull] PConversionParserParser.ExprNameContext context)
        {
            return new NameReferenceExpression(context, context.name.GetText());
        }

        public override IExpression VisitExprProp([NotNull] PConversionParserParser.ExprPropContext context)
        {
            return new PropertyReferenceExpression(context, context.expr().Accept(this), context.NAME().GetText());
        }

        public override IExpression VisitExprFunction([NotNull] PConversionParserParser.ExprFunctionContext context)
        {
            return new CallExpression(context, context.func().NAME().GetText(), context.func().expr().Select(p => p.Accept(this)).ToList());
        }

        public override IExpression VisitExprMethod([NotNull] PConversionParserParser.ExprMethodContext context)
        {
            return new CallExpression(context, context.func().name.Text, context.func().expr().Select(p => p.Accept(this)).Prepend(context.target.Accept(this)).ToList());
        }

        public override IExpression VisitExprOperator([NotNull] PConversionParserParser.ExprOperatorContext context)
        {
            return new CallExpression(context, context.op.Text, context.expr().Select(p => p.Accept(this)).ToList());
        }

        public override IExpression VisitExprNot([NotNull] PConversionParserParser.ExprNotContext context)
        {
            return new CallExpression(context, context.NOT().GetText(), new List<IExpression> { context.expr().Accept(this) });
        }

        public override IExpression VisitExprParenthesis([NotNull] PConversionParserParser.ExprParenthesisContext context)
        {
            return context.expr().Accept(this);
        }
    }
}

