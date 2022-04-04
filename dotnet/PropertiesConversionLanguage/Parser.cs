using System;
using System.IO;
using Antlr4.Runtime;
using Genexus.PropertiesLanguage.Conversion.Antlr;

namespace Genexus.PropertiesLanguage.Conversion
{
    public class Parser
    {
        public static Model Parse(string example)
        {
            var charStream = new AntlrInputStream(example);
            var lexer = new PConversionParserLexer(charStream);
            var tokenStream = new CommonTokenStream(lexer);
            var output = new StringWriter();
            var error = new StringWriter();
            var parser = new PConversionParserParser(tokenStream, output, error);
            var tree = parser.conversions();
            var s = output.ToString() + error.ToString();
            if (!string.IsNullOrEmpty(s))
                throw new Exception(s);

            var model = tree.Accept(ModelVisitor.Instance);
            return model;
        }
    }
}

