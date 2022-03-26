using System;
using System.Collections.Generic;
using Antlr4.Runtime;
using Genexus.PropertiesLanguage;

namespace Genexus.PropertiesInstanceLanguage
{
    public class Model
    {
        public MObject Root;
    }

    public partial class MContext
    {
        public IToken StartToken;
        public IToken StopToken;

		public int Start
		{
			get { return StartToken.StartIndex; }
		}

		public int End
		{
			get { return StopToken.StopIndex; }
		}
	}

	public partial class MBase
    {
        public MBase(MContext context)
        {
            Context = context;
        }

        public MContext Context { get; private set; }
    }

    public partial class MObject : MBase
    {
        public MObject(MContext context) : base(context) { }

        public IList<MMaping> Mappings;
		public IToken OpenBracketToken;
	}

	public class MMaping
	{
		public string Name;
		public MBase Value;
		public IToken CommaToken;
		public PropertyDefinition Definition;
	}

	public partial class MList : MBase
    {
        public MList(MContext context) : base(context) { }

        public IList<MBase> Values;
		public string InnerText;
	}

    public partial class MConstant : MBase
    {
        public MConstant(MContext context) : base(context) { }

        public object Value;
    }

    public partial class MInt : MConstant
    {
        public MInt(MContext context) : base(context) { }
    }

    public partial class MBool : MConstant
    {
        public MBool(MContext context) : base(context) { }
    }

    public partial class MString : MConstant
    {
        public MString(MContext context) : base(context) { }
    }
}
