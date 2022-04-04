using System.Collections.Generic;
using Antlr4.Runtime;

namespace Genexus.PropertiesLanguage.Definition
{
    public partial class Model
    {
        public IList<string> ImportList;
        public IList<FlagsList> FlagsList;
        public IList<PType> TypesList;
	}

    public partial class FlagsList
    {
        public string Name;
        public IList<FlagDefinition> Flags;
    }

    public partial class FlagDefinition
    {
        public string Name;
        public string Type;
        public string RuntimeName;
        public bool Deprecated;
    }

    public partial class PType
    {
        public string? Name;
        public string? ExtendsType;
        public IExpression? Condition;
        public IList<PropertyDefinition> Properties;
		public IToken StartToken;
		public IToken StopToken;
		public IToken OpenBracketToken;
	}

	public partial class PropertyDefinition
    {
        public string Name;
        public string Type;
		public bool IsCollection;
		public string Description;
		public IExpression Default;
        public IExpression Apply;
        public IExpression Readonly;
        public IExpression Valid;
		public bool IsTypeChanged;
		public bool IsCollectionChanged;
		public IToken StartToken;
		public IToken StopToken;
		public IToken TypeToken;
		public IToken OpenBracketToken;
		public IToken IsCollectionStartToken;
		public IToken IsCollectionStopToken;
	}
}
