using System;
using System.IO;
using Antlr4.Runtime;
using Genexus.Language.PropertiesDefinition.Antlr;

namespace Genexus.Language.PropertiesDefinition
{
    public class Parser
    {
        public static Model Parse(string text)
		{
			return Parse(GetTokenStream(text));
		}

		public static Model Parse(ITokenStream tokenStream)
        {
            var output = new StringWriter();
            var error = new StringWriter();
            var parser = new PDefinitionParserParser(tokenStream, output, error);
            var tree = parser.definitions();
            var s = output.ToString() + error.ToString();
            if (!string.IsNullOrEmpty(s))
                throw new Exception(s);

            var model = tree.Accept(ModelVisitor.Instance);            
            return model;
        }

		public static ITokenStream GetTokenStream(string text)
		{
			var charStream = new AntlrInputStream(text);
			var lexer = new PDefinitionParserLexer(charStream);
			return new CommonTokenStream(lexer);			
		}
	}
}
