using PropertiesLanguage;
using System.IO;
using Xunit;

namespace Test
{
    public class ParserUnitTest
    {
        [Fact]
        public void TestFile()
        {
            var text = File.ReadAllText("test.p");
            var model = Parser.Parse(text);
            Assert.Equal(6, model.Definitions.Count);
        }
    }
}