using System;
using System.IO;
using Antlr4.Runtime;
using Genexus.PropertiesLanguage.Instance.Antlr;

namespace Genexus.PropertiesLanguage.Instance
{
    public class Parser
    {
        public static Model Parse(string example)
        {
            var charStream = new AntlrInputStream(example);
            var lexer = new PInstanceParserLexer(charStream);
            var tokenStream = new CommonTokenStream(lexer);
            var output = new StringWriter();
            var error = new StringWriter();
            var parser = new PInstanceParserParser(tokenStream, output, error);
            var tree = parser.instance();
            var s = output.ToString() + error.ToString();
            if (!string.IsNullOrEmpty(s))
                throw new Exception(s);

            var model = tree.Accept(ModelVisitor.Instance);
            return model;
        }
    }
}
