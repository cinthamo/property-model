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
            var parser = new PropsParser(tokenStream);
            var tree = parser.definitions();
            var model = tree.Accept(ModelVisitor.Instance);            
            return model;
        }
    }
}