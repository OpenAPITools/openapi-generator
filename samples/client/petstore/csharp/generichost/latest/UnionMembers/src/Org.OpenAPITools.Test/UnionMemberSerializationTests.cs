using System.Linq;
using System.Text.Json;
using System.Text.Json.Nodes;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Extensions;
using Org.OpenAPITools.Model;
using Xunit;

namespace Org.OpenAPITools.Test
{
    public class UnionMemberSerializationTests
    {
        private readonly JsonSerializerOptions _options = Host.CreateDefaultBuilder([])
            .ConfigureApi((context, options) => { })
            .Build()
            .Services.GetRequiredService<JsonSerializerOptionsProvider>()
            .Options;

        [Theory]
        // https://github.com/OpenAPITools/openapi-generator/issues/23261
        [InlineData("""{"status":"active"}""")]
        [InlineData("""{"status":"frozen"}""")]
        [InlineData("""{"nullableStatus":"inactive"}""")]
        [InlineData("""{"nullableStatus":"frozen"}""")]
        [InlineData("""{"statusOrText":"frozen"}""")]
        [InlineData("""{"patternedText":"abc"}""")]
        [InlineData("""{"flexibleValue":true}""")]
        [InlineData("""{"flexibleValue":"text"}""")]
        [InlineData("""{"flexibleValue":{"a":1}}""")]
        [InlineData("""{"flexibleValue":{}}""")]
        [InlineData("""{"flexibleValue":["a","b"]}""")]
        // https://github.com/OpenAPITools/openapi-generator/issues/23529
        [InlineData("""{"fooOrList":{"id":1}}""")]
        [InlineData("""{"fooOrList":[{"id":1},{"id":2}]}""")]
        // https://github.com/OpenAPITools/openapi-generator/issues/23693
        [InlineData("""{"entry":{"id":"d1","resourceType":"Diagnostic"}}""")]
        [InlineData("""{"entry":{"id":"o1","resourceType":"Observation"}}""")]
        [InlineData("""{"fruit":{"color":"red","cultivar":"gala"}}""")]
        [InlineData("""{"fruit":{"color":"yellow","lengthCm":18.5}}""")]
        // https://github.com/OpenAPITools/openapi-generator/issues/25154
        [InlineData("""{"asset":{"id":1,"imageUri":"https://example.com/i.jpg","type":"image"}}""")]
        [InlineData("""{"asset":{"documentUri":"https://example.com/d.pdf","id":2,"type":"document"}}""")]
        // object members of an anyOf without a discriminator are still merged
        [InlineData("""{"pet":{"meow":true}}""")]
        [InlineData("""{"pet":{}}""")]
        public void RoundTrips(string json)
        {
            Holder holder = JsonSerializer.Deserialize<Holder>(json, _options)!;

            string output = JsonSerializer.Serialize(holder, _options);

            Assert.True(JsonNode.DeepEquals(JsonNode.Parse(json), JsonNode.Parse(output)), output);
        }

        [Fact]
        public void KnownValueSetsEveryMatchingMember()
        {
            OpenStatus status = JsonSerializer.Deserialize<OpenStatus>("\"active\"", _options)!;

            Assert.Equal(Status.Active, status.Status);
            Assert.Equal("active", status.String);
        }

        [Fact]
        public void UnknownValueSetsOnlyTheStringMember()
        {
            OpenStatus status = JsonSerializer.Deserialize<OpenStatus>("\"frozen\"", _options)!;

            Assert.Null(status.Status);
            Assert.Equal("frozen", status.String);
        }

        [Theory]
        [InlineData("""{"status":5}""")]
        [InlineData("""{"status":[1,2]}""")]
        [InlineData("""{"fooOrList":"foo"}""")]
        [InlineData("""{"pet":"cat"}""")]
        [InlineData("""{"entry":"d1"}""")]
        [InlineData("""{"flexibleValue":5}""")]
        [InlineData("""{"asset":5}""")]
        public void ValueWithNoMatchingMemberThrows(string json)
        {
            Assert.Throws<JsonException>(() => JsonSerializer.Deserialize<Holder>(json, _options));
        }

        [Fact]
        public void ArraySetsTheListMember()
        {
            FooOrList fooOrList = JsonSerializer.Deserialize<FooOrList>("""[{"id":1},{"id":2}]""", _options)!;

            Assert.Null(fooOrList.Foo);
            Assert.Equal(new int?[] { 1, 2 }, fooOrList.List!.Select(foo => foo.Id));
        }

        [Fact]
        public void ArrayIsNotReadIntoAFreeFormObjectMember()
        {
            FlexibleValue value = JsonSerializer.Deserialize<FlexibleValue>("""["a","b"]""", _options)!;

            Assert.Null(value.Object);
            Assert.Equal(new[] { "a", "b" }, value.List);
        }
    }
}
