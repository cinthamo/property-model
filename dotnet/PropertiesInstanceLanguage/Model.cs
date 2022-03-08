using System.Collections.Generic;
using Antlr4.Runtime;

namespace Genexus.PropertiesInstanceLanguage
{
    public struct Model
    {
        public MObject Root;
    }

    public struct MObject
    {
        public IDictionary<string, object> Mappings;
    }

    public struct MList
    {
        public IList<object> Values;
    }
}
