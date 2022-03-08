using System;
using System.Collections.Generic;
using System.Linq;
using Antlr4.Runtime.Misc;
using Antlr4.Runtime.Tree;
using Genexus.PropertiesInstanceLanguage.Antlr;

namespace Genexus.PropertiesInstanceLanguage
{
    public class ModelVisitor : PInstParserBaseVisitor<Model>
    {
        public static readonly ModelVisitor Instance = new();

        public override Model VisitInstance([NotNull] PInstParserParser.InstanceContext context)
        {
            return new Model
            {
                Root = context.@object().Accept(ObjectVisitor.Instance)
            };
        }
    }

    public class ObjectVisitor : PInstParserBaseVisitor<MObject>
    {
        public static readonly ObjectVisitor Instance = new();

        public override MObject VisitObject([NotNull] PInstParserParser.ObjectContext context)
        {
            return new MObject
            {
                Mappings = context.mapping()
                    .Select(p => p.Accept(MappingVisitor.Instance))
                    .ToDictionary(k => k.Key, v => v.Value)
            };
        }
    }

    public class MappingVisitor : PInstParserBaseVisitor<KeyValuePair<string, object>>
    {
        public static readonly MappingVisitor Instance = new();

        public override KeyValuePair<string, object> VisitMapping([NotNull] PInstParserParser.MappingContext context)
        {
            return new KeyValuePair<string, object>(
                context.key().GetText(), context.value().Accept(ValueVisitor.Instance)
            );
        }
    }

    public class ValueVisitor : PInstParserBaseVisitor<object>
    {
        public static readonly ValueVisitor Instance = new();

        public override object VisitValueNumber([NotNull] PInstParserParser.ValueNumberContext context)
        {
            return int.Parse(context.NUMBER().GetText());
        }

        public override object VisitValueBool([NotNull] PInstParserParser.ValueBoolContext context)
        {
            return context.BOOL().GetText() == "true";
        }

        public override object VisitValueString([NotNull] PInstParserParser.ValueStringContext context)
        {
            var value = context.STRING().GetText();
            return value.Substring(1, value.Length - 2);
        }

        public override object VisitValueObject([NotNull] PInstParserParser.ValueObjectContext context)
        {
            return context.@object().Accept(ObjectVisitor.Instance);
        }

        public override object VisitValueList([NotNull] PInstParserParser.ValueListContext context)
        {
            return context.list().Accept(ListVisitor.Instance);
        }
    }

    public class ListVisitor : PInstParserBaseVisitor<MList>
    {
        public static readonly ListVisitor Instance = new();

        public override MList VisitList([NotNull] PInstParserParser.ListContext context)
        {
            return new MList {
                Values = context.value().Select(p => p.Accept(ValueVisitor.Instance)).ToList()
            };
        }
    }
}
