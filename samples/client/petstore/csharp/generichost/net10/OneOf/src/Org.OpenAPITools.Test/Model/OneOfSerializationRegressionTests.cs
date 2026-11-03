using System.Text.Json;
using Microsoft.Extensions.DependencyInjection;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;
using Xunit;

namespace Org.OpenAPITools.Test.Model
{
    public class OneOfSerializationRegressionTests
    {
        [Fact]
        public void WritesActiveObjectVariantAndWrapperProperties()
        {
            JsonSerializerOptions? options = null;
            new HostConfiguration(new ServiceCollection()).ConfigureJsonOptions(value => options = value);
            Assert.NotNull(options);
            Fruit fruit = JsonSerializer.Deserialize<Fruit>("{\"kind\":\"gala\",\"color\":\"red\"}", options)!;
            Assert.NotNull(fruit.Apple);
            using (JsonDocument json = JsonDocument.Parse(JsonSerializer.Serialize(fruit, options)))
            {
                Assert.Equal("gala", json.RootElement.GetProperty("kind").GetString());
                Assert.Equal("red", json.RootElement.GetProperty("color").GetString());
            }

            fruit.Apple = null;
            fruit.Banana = new Banana { Count = 2 };
            using (JsonDocument json = JsonDocument.Parse(JsonSerializer.Serialize(fruit, options)))
            {
                Assert.Equal(2, json.RootElement.GetProperty("count").GetInt32());
                Assert.Equal("red", json.RootElement.GetProperty("color").GetString());
            }
        }
    }
}
