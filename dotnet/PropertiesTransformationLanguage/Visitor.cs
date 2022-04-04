using System;
using System.Collections.Generic;
using System.Linq;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using Genexus.PropertiesLanguage.Definition;
using Genexus.PropertiesLanguage.Transformation.Antlr;

namespace Genexus.PropertiesLanguage.Transformation
{
    public class ModelVisitor : PTransformationParserBaseVisitor<Model>
    {
        public static readonly ModelVisitor Instance = new();

        public override Model VisitConversions([NotNull] PTransformationParserParser.ConversionsContext context)
        {
            return new Model
            {
                ImportList = context.importt().Select(i => i.name.Text).ToList(),
                ConversionList = context.conversion().Select(f => f.Accept(ConversionVisitor.Instance)).ToList(),
                RewriteList = context.rewrite().Select(r => r.Accept(RewriteVisitor.Instance)).ToList(),
                TransformList = context.transform().Select(t => t.Accept(TransformVisitor.Instance)).ToList()
            };
        }
    }

    public class ConversionVisitor : PTransformationParserBaseVisitor<Conversion>
    {
        public static readonly ConversionVisitor Instance = new();

        public override Conversion VisitConversion([NotNull] PTransformationParserParser.ConversionContext context)
        {
            return new Conversion
            {
                Name = context.namefromto().name.Text,
                From = context.namefromto().from.Text,
                To = context.namefromto().to.Text,
                RuleList = context.crule().Select(r => r.Accept(CRuleVisitor.Instance)).ToList()
            };
        }
    }

    public class CRuleVisitor : PTransformationParserBaseVisitor<CRule>
    {
        public static readonly CRuleVisitor Instance = new();

        public override CRule VisitCrule([NotNull] PTransformationParserParser.CruleContext context)
        {
            return new CRule
            {
                Name = context.name.Text,
                Expression = context.expr().Accept(ExpressionVisitor.Instance)
            };
        }
    }

    public class RewriteVisitor : PTransformationParserBaseVisitor<Rewrite>
    {
        public static readonly RewriteVisitor Instance = new();

        public override Rewrite VisitRewrite([NotNull] PTransformationParserParser.RewriteContext context)
        {
            return new Rewrite()
            {
                Name = context.namefromto().name.Text,
                From = context.namefromto().from.Text,
                To = context.namefromto().to.Text,
                RuleList = context.rrule().Select(r => r.Accept(RRuleVisitor.Instance)).ToList()
            };
        }
    }

    public class RRuleVisitor : PTransformationParserBaseVisitor<RRule>
    {
        public static readonly RRuleVisitor Instance = new();

        public override RRule VisitRrule([NotNull] PTransformationParserParser.RruleContext context)
        {
            return new RRule()
            {
                From = context.from.Text,
                To = context.to.Text
            };
        }
    }

    public class TransformVisitor : PTransformationParserBaseVisitor<Transform>
    {
        public static readonly TransformVisitor Instance = new();

        public override Transform VisitTransform([NotNull] PTransformationParserParser.TransformContext context)
        {
            return new Transform()
            {
                Name = context.namefromto().name.Text,
                From = context.namefromto().from.Text,
                To = context.namefromto().to.Text,
                TransformList = context.trule().Select(t => t.name.Text).ToList()
            };
        }
    }

    // Copied from PropertiesDefinitionLanguage/Visitor.cs
    public class ExpressionVisitor : PTransformationParserBaseVisitor<IExpression>
    {
        public static readonly ExpressionVisitor Instance = new();

        public override IExpression VisitExprNumber([NotNull] PTransformationParserParser.ExprNumberContext context)
        {
            return new NumericExpression(context, int.Parse(context.NUMBER().GetText()));
        }

        public override IExpression VisitExprBool([NotNull] PTransformationParserParser.ExprBoolContext context)
        {
            var value = context.BOOL().GetText() == "true";
            return new BooleanExpression(context, value);
        }

        public override IExpression VisitExprString([NotNull] PTransformationParserParser.ExprStringContext context)
        {
            var value = context.STRING_DOUBLE().GetText();
            return new StringExpression(context, value.Substring(1, value.Length - 2)); // remove quotes
        }

        public override IExpression VisitExprNull([NotNull] PTransformationParserParser.ExprNullContext context)
        {
            return new NullExpression(context);
        }

        public override IExpression VisitExprValue([NotNull] PTransformationParserParser.ExprValueContext context)
        {
            return new ValueReferenceExpression(context);
        }

        public override IExpression VisitExprName([NotNull] PTransformationParserParser.ExprNameContext context)
        {
            return new NameReferenceExpression(context, context.name.GetText());
        }

        public override IExpression VisitExprProp([NotNull] PTransformationParserParser.ExprPropContext context)
        {
            return new PropertyReferenceExpression(context, context.expr().Accept(this), context.NAME().GetText());
        }

        public override IExpression VisitExprFunction([NotNull] PTransformationParserParser.ExprFunctionContext context)
        {
            return new CallExpression(context, context.func().NAME().GetText(), context.func().expr().Select(p => p.Accept(this)).ToList());
        }

        public override IExpression VisitExprMethod([NotNull] PTransformationParserParser.ExprMethodContext context)
        {
            return new CallExpression(context, context.func().name.Text, context.func().expr().Select(p => p.Accept(this)).Prepend(context.target.Accept(this)).ToList());
        }

        public override IExpression VisitExprOperator([NotNull] PTransformationParserParser.ExprOperatorContext context)
        {
            return new CallExpression(context, context.op.Text, context.expr().Select(p => p.Accept(this)).ToList());
        }

        public override IExpression VisitExprNot([NotNull] PTransformationParserParser.ExprNotContext context)
        {
            return new CallExpression(context, context.NOT().GetText(), new List<IExpression> { context.expr().Accept(this) });
        }

        public override IExpression VisitExprParenthesis([NotNull] PTransformationParserParser.ExprParenthesisContext context)
        {
            return context.expr().Accept(this);
        }
    }
}
