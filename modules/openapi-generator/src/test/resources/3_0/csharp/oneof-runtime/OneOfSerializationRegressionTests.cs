using System;
using System.Text.Json;
using System.Text.Json.Serialization;
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

        private static Fruit CreateFruit()
        {
            JsonSerializerOptions? options = null;
            new HostConfiguration(new ServiceCollection()).ConfigureJsonOptions(value => options = value);
            return JsonSerializer.Deserialize<Fruit>("{\"kind\":\"gala\"}", options!)!;
        }

        [Fact]
        public void RejectsMultipleVariants()
        {
            Fruit fruit = CreateFruit();
            fruit.Banana = new Banana { Count = 2 };
            var options = new JsonSerializerOptions();
            options.Converters.Add(new FruitJsonConverter());
            Assert.Throws<JsonException>(() => JsonSerializer.Serialize(fruit, options));
        }

        [Fact]
        public void UsesGeneratedVariantConverterWhenNotRegistered()
        {
            Fruit fruit = CreateFruit();
            var options = new JsonSerializerOptions();
            options.Converters.Add(new FruitJsonConverter());
            using JsonDocument json = JsonDocument.Parse(JsonSerializer.Serialize(fruit, options));
            Assert.Equal("gala", json.RootElement.GetProperty("kind").GetString());
        }

        [Fact]
        public void UsesCustomVariantConverter()
        {
            Fruit fruit = CreateFruit();
            var options = new JsonSerializerOptions();
            options.Converters.Add(new FruitJsonConverter());
            options.Converters.Add(new CustomAppleConverter());
            using JsonDocument json = JsonDocument.Parse(JsonSerializer.Serialize(fruit, options));
            Assert.Equal("custom", json.RootElement.GetProperty("kind").GetString());
        }

        [Fact]
        public void UsesDerivedVariantConverter()
        {
            Fruit fruit = CreateFruit();
            fruit.Apple = new FancyApple();
            var options = new JsonSerializerOptions();
            options.Converters.Add(new FruitJsonConverter());
            options.Converters.Add(new FancyAppleConverter());
            using JsonDocument json = JsonDocument.Parse(JsonSerializer.Serialize(fruit, options));
            Assert.Equal("fancy", json.RootElement.GetProperty("extra").GetString());
        }

        private sealed class CustomAppleConverter : JsonConverter<Apple>
        {
            public override Apple Read(ref Utf8JsonReader reader, Type type, JsonSerializerOptions options) => throw new NotSupportedException();
            public override void Write(Utf8JsonWriter writer, Apple value, JsonSerializerOptions options)
            {
                writer.WriteStartObject(); writer.WriteString("kind", "custom"); writer.WriteEndObject();
            }
        }

        private sealed class FancyApple : Apple { }

        private sealed class FancyAppleConverter : JsonConverter<FancyApple>
        {
            public override FancyApple Read(ref Utf8JsonReader reader, Type type, JsonSerializerOptions options) => throw new NotSupportedException();
            public override void Write(Utf8JsonWriter writer, FancyApple value, JsonSerializerOptions options)
            {
                writer.WriteStartObject(); writer.WriteString("extra", "fancy"); writer.WriteEndObject();
            }
        }
    }
}
