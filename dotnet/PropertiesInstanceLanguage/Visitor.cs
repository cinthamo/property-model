using System.Collections.Generic;
using System.Linq;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using Genexus.PropertiesInstanceLanguage.Antlr;

namespace Genexus.PropertiesInstanceLanguage
{
	public static class Extension
	{
		public static MContext GetContext(this ParserRuleContext context)
		{
			return new MContext()
			{
				StartToken = context.Start,
				StopToken = context.Stop
			};
		}
	}	

    public class ModelVisitor : PInstParserBaseVisitor<Model>
    {
        public static readonly ModelVisitor Instance = new();

        public override Model VisitInstance([NotNull] PInstParserParser.InstanceContext context)
        {
            return new Model
            {
                Root = context.@object() == null ?
                    new MObject(null) :
                    context.@object().Accept(ObjectVisitor.Instance)
            };
        }
    }

    public class ObjectVisitor : PInstParserBaseVisitor<MObject>
    {
        public static readonly ObjectVisitor Instance = new();

		public override MObject VisitObject([NotNull] PInstParserParser.ObjectContext context)
		{
			var mappings = context.mapping()
					.Select(p => p.Accept(MappingVisitor.Instance))
					.ToList();
			for (int i = 0; i < context.COMMA().Length; i++)
				mappings[i].CommaToken = context.COMMA()[i].Symbol;

			return new MObject(context.GetContext())
            {
                Mappings = mappings,
				OpenBracketToken = context.open
            };
        }
    }

    public class MappingVisitor : PInstParserBaseVisitor<MMaping>
    {
        public static readonly MappingVisitor Instance = new();

        public override MMaping VisitMapping([NotNull] PInstParserParser.MappingContext context)
        {
            return new MMaping() {
				Name = context.key().Accept(KeyVisitor.Instance),
				Value = context.value().Accept(ValueVisitor.Instance)
			};
        }
    }

	public class KeyVisitor : PInstParserBaseVisitor<string>
	{
		public static readonly KeyVisitor Instance = new();

		public override string VisitKeyName([NotNull] PInstParserParser.KeyNameContext context)
		{
			return context.NAME().GetText();
		}

		public override string VisitKeyString([NotNull] PInstParserParser.KeyStringContext context)
		{
			var value = context.STRING().GetText();
			return value.Substring(1, value.Length - 2);
		}
	}

    public class ValueVisitor : PInstParserBaseVisitor<MBase>
    {
        public static readonly ValueVisitor Instance = new();

        public override MBase VisitValueNumber([NotNull] PInstParserParser.ValueNumberContext context)
        {
			return new MInt(context.GetContext())
			{
				Value = int.Parse(context.NUMBER().GetText())
			};
        }

		public override MBase VisitValueBool([NotNull] PInstParserParser.ValueBoolContext context)
		{
			return new MBool(context.GetContext())
			{
				Value = context.BOOL().GetText() == "true"
			};
        }

		public override MBase VisitValueString([NotNull] PInstParserParser.ValueStringContext context)
		{
			var value = context.STRING().GetText();
			return new MString(context.GetContext())
			{
				Value = value.Substring(1, value.Length - 2)
			};
        }

		public override MBase VisitValueName([NotNull] PInstParserParser.ValueNameContext context)
		{
			return new MString(context.GetContext())
			{
				Value = context.NAME().GetText()
			};
		}

		public override MBase VisitValueObject([NotNull] PInstParserParser.ValueObjectContext context)
        {
            return context.@object().Accept(ObjectVisitor.Instance);
        }

        public override MBase VisitValueList([NotNull] PInstParserParser.ValueListContext context)
        {
            return context.list().Accept(ListVisitor.Instance);
        }
    }

    public class ListVisitor : PInstParserBaseVisitor<MList>
    {
        public static readonly ListVisitor Instance = new();

        public override MList VisitList([NotNull] PInstParserParser.ListContext context)
        {
			var text = context.GetText();
			return new MList(context.GetContext()) {
				Values = context.value().Select(p => p.Accept(ValueVisitor.Instance)).ToList(),
				InnerText = text.Substring(1, text.Length - 2).Trim()
            };
        }
    }
}