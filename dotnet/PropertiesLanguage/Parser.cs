using Antlr4.Runtime;
using PropertiesLanguage.Antlr;

namespace PropertiesLanguage
{
    public class Parser
    {
        public static Model Parse(string example)
        {
            var charStream = new AntlrInputStream(example);
            var lexer = new PropsLexer(charStream);
            var tokenStream = new CommonTokenStream(lexer);
            var output = new StringWriter();
            var error = new StringWriter();
            var parser = new PropsParser(tokenStream, output, error);
            var tree = parser.definitions();
            var s = output.ToString() + error.ToString();
            if (!string.IsNullOrEmpty(s))
                throw new Exception(s);

            var model = tree.Accept(ModelVisitor.Instance);            
            return model;
        }
    }
}
