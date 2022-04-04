using System;
using System.IO;
using Antlr4.Runtime;
using Genexus.PropertiesLanguage.Transformation.Antlr;

namespace Genexus.PropertiesLanguage.Transformation
{
    public class Parser
    {
        public static Model Parse(string example)
        {
            var charStream = new AntlrInputStream(example);
            var lexer = new PTransformationParserLexer(charStream);
            var tokenStream = new CommonTokenStream(lexer);
            var output = new StringWriter();
            var error = new StringWriter();
            var parser = new PTransformationParserParser(tokenStream, output, error);
            var tree = parser.conversions();
            var s = output.ToString() + error.ToString();
            if (!string.IsNullOrEmpty(s))
                throw new Exception(s);

            var model = tree.Accept(ModelVisitor.Instance);
            return model;
        }
    }
}

