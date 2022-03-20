using System.Collections.Generic;
using Antlr4.Runtime;

namespace Genexus.PropertiesInstanceLanguage
{
    public class Model
    {
        public MObject Root;
    }

    public class MContext
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

        public IList<KeyValuePair<string, MBase>> Mappings;
    }

    public partial class MList : MBase
    {
        public MList(MContext context) : base(context) { }

        public IList<MBase> Values;
    }

    public partial class MConstant : MBase
    {
        public MConstant(MContext context) : base(context) { }

        public object Value;
    }

    public partial class MInt : MConstant
    {
        public MInt(MContext context): base(context) { }
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
