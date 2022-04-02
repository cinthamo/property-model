using System.Collections.Generic;
using System.Linq;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using Genexus.PropertiesLanguage.Instance.Antlr;

namespace Genexus.PropertiesLanguage.Instance
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

    public class ModelVisitor : PInstanceParserBaseVisitor<Model>
    {
        public static readonly ModelVisitor Instance = new();

        public override Model VisitInstance([NotNull] PInstanceParserParser.InstanceContext context)
        {
            return new Model
            {
                Root = context.@object() == null ?
                    new MObject(context.GetContext()) :
                    context.@object().Accept(ObjectVisitor.Instance)
            };
        }
    }

    public class ObjectVisitor : PInstanceParserBaseVisitor<MObject>
    {
        public static readonly ObjectVisitor Instance = new();

		public override MObject VisitObject([NotNull] PInstanceParserParser.ObjectContext context)
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

    public class MappingVisitor : PInstanceParserBaseVisitor<MMaping>
    {
        public static readonly MappingVisitor Instance = new();

        public override MMaping VisitMapping([NotNull] PInstanceParserParser.MappingContext context)
        {
            return new MMaping() {
				Name = context.key().Accept(KeyVisitor.Instance),
				Value = context.value().Accept(ValueVisitor.Instance)
			};
        }
    }

	public class KeyVisitor : PInstanceParserBaseVisitor<string>
	{
		public static readonly KeyVisitor Instance = new();

		public override string VisitKeyName([NotNull] PInstanceParserParser.KeyNameContext context)
		{
			return context.NAME().GetText();
		}

		public override string VisitKeyString([NotNull] PInstanceParserParser.KeyStringContext context)
		{
			var value = context.@string().GetText();
			return value.Substring(1, value.Length - 2);
		}
	}

    public class ValueVisitor : PInstanceParserBaseVisitor<MBase>
    {
        public static readonly ValueVisitor Instance = new();

        public override MBase VisitValueNumber([NotNull] PInstanceParserParser.ValueNumberContext context)
        {
			return new MInt(context.GetContext())
			{
				Value = int.Parse(context.NUMBER().GetText())
			};
        }

		public override MBase VisitValueBool([NotNull] PInstanceParserParser.ValueBoolContext context)
		{
			return new MBool(context.GetContext())
			{
				Value = context.BOOL().GetText() == "true"
			};
        }

		public override MBase VisitValueString([NotNull] PInstanceParserParser.ValueStringContext context)
		{
			var value = context.@string().GetText();
			return new MString(context.GetContext())
			{
				Value = value.Substring(1, value.Length - 2)
			};
        }

		public override MBase VisitValueName([NotNull] PInstanceParserParser.ValueNameContext context)
		{
			return new MString(context.GetContext())
			{
				Value = context.NAME().GetText()
			};
		}

		public override MBase VisitValueObject([NotNull] PInstanceParserParser.ValueObjectContext context)
        {
            return context.@object().Accept(ObjectVisitor.Instance);
        }

        public override MBase VisitValueList([NotNull] PInstanceParserParser.ValueListContext context)
        {
            return context.list().Accept(ListVisitor.Instance);
        }
    }

    public class ListVisitor : PInstanceParserBaseVisitor<MList>
    {
        public static readonly ListVisitor Instance = new();

        public override MList VisitList([NotNull] PInstanceParserParser.ListContext context)
        {
			var text = context.GetText();
			return new MList(context.GetContext()) {
				Values = context.value().Select(p => p.Accept(ValueVisitor.Instance)).ToList(),
				InnerText = text.Substring(1, text.Length - 2).Trim()
            };
        }
    }
}
