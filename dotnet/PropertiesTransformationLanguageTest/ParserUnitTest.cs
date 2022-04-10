using System.IO;
using Genexus.PropertiesLanguage.Transformation;
using Xunit;

namespace PropertiesTransformationLanguageTest;

public class ParserUnitTest
{
    [Fact]
    public void TestFile()
    {
        var text = File.ReadAllText("test.pt");
        var model = Parser.Parse(text);
        Assert.Equal(2, model.ConversionList.Count);
    }
}
