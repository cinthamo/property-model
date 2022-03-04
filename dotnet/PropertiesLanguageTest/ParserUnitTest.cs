using System.IO;
using Xunit;

namespace PropertiesLanguage.Test
{
    public class ParserUnitTest
    {
        [Fact]
        public void TestFile()
        {
            var text = File.ReadAllText("test.p");
            var model = Parser.Parse(text);
            Assert.Equal(7, model.Definitions.Count);

            foreach (var definitions in model.Definitions)
            {
                var error = new StringWriter();
                new Checker(error).CheckTypes(new TestTypeContext(definitions), definitions);
                Assert.Equal("", error.ToString());
            }
        }
    }
}
