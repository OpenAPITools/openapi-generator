using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Extensions;
using Org.OpenAPITools.Model;
using System.Text.Json;

namespace OpenAPIClient_generichost_manual_tests
{
    [TestClass]
    public sealed class SerializationTests
    {
        private readonly IHost _host;
        private readonly JsonSerializerOptions _jsonSerializerOptions;

        public SerializationTests()
        {
            IHostBuilder hostBuild = Host.CreateDefaultBuilder(Array.Empty<string>()).ConfigureApi((context, services, options) =>
            {
                string apiKeyTokenValue = context.Configuration["<token>"] ?? "Token not found.";
                ApiKeyToken apiKeyToken = new(apiKeyTokenValue, ClientUtils.ApiKeyHeader.Api_key, timeout: TimeSpan.FromSeconds(1));
                options.AddTokens(apiKeyToken);

                string bearerTokenValue = context.Configuration["<token>"] ?? "Token not found.";
                BearerToken bearerToken = new(bearerTokenValue, timeout: TimeSpan.FromSeconds(1));
                options.AddTokens(bearerToken);

                string basicTokenUsername = context.Configuration["<username>"] ?? "Username not found.";
                string basicTokenPassword = context.Configuration["<password>"] ?? "Password not found.";
                BasicToken basicToken = new(basicTokenUsername, basicTokenPassword, timeout: TimeSpan.FromSeconds(1));
                options.AddTokens(basicToken);

                HttpSigningConfiguration config = new("<keyId>", "<keyFilePath>", null, new List<string>(), System.Security.Cryptography.HashAlgorithmName.SHA256, "<signingAlgorithm>", 0);
                HttpSignatureToken httpSignatureToken = new(config, timeout: TimeSpan.FromSeconds(1));
                options.AddTokens(httpSignatureToken);

                string oauthTokenValue = context.Configuration["<token>"] ?? "Token not found.";
                OAuthToken oauthToken = new(oauthTokenValue, timeout: TimeSpan.FromSeconds(1));
                options.AddTokens(oauthToken);
            });

            _host = hostBuild.Build();

            _jsonSerializerOptions = _host.Services.GetRequiredService<JsonSerializerOptionsProvider>().Options;
        }

        [TestMethod]
        public void Apple()
        {
            string expected = """{"color_code":"#000000","cultivar":"cultivar","origin":"origin"}""";
            Apple? apple = JsonSerializer.Deserialize<Apple>(expected, _jsonSerializerOptions);
            string actual = JsonSerializer.Serialize(apple, _jsonSerializerOptions);
            Assert.AreEqual(expected, actual);
        }

        [TestMethod]
        public void Pig()
        {
            string expected = """{"className":"BasquePig"}""";
            Pig? pig = JsonSerializer.Deserialize<Pig>(expected, _jsonSerializerOptions);
            string actual = JsonSerializer.Serialize(pig, _jsonSerializerOptions);
            Assert.AreEqual(expected, actual);
        }

        [TestMethod]
        public void DanishPig()
        {
            string expected = """{"className":"DanishPig"}""";
            DanishPig? danishPig = JsonSerializer.Deserialize<DanishPig>(expected, _jsonSerializerOptions);
            string actual = JsonSerializer.Serialize(danishPig, _jsonSerializerOptions);
            Assert.AreEqual(expected, actual);
        }

        [TestMethod]
        public void GmFruit()
        {
            string expected = """{"color_code":"#000000","cultivar":"cultivar","origin":"origin","lengthCm":10}""";
            GmFruit? gmFruit = JsonSerializer.Deserialize<GmFruit>(expected, _jsonSerializerOptions);
            string actual = JsonSerializer.Serialize(gmFruit, _jsonSerializerOptions);
            Assert.AreEqual(expected, actual);

            Banana? banana = JsonSerializer.Deserialize<Banana>(expected, _jsonSerializerOptions);
            string bananaActual = JsonSerializer.Serialize(banana);
            string bananaExpected = """{"lengthCm":10}""";
            Assert.AreEqual(bananaExpected, bananaActual);

            Apple? apple = JsonSerializer.Deserialize<Apple>(expected, _jsonSerializerOptions);
            string appleActual = JsonSerializer.Serialize(apple);
            string appleExpected = """{"color_code":"#000000","cultivar":"cultivar","origin":"origin"}""";
            Assert.AreEqual(appleExpected, appleActual);
        }

        [TestMethod]
        public void EquilateralTriangle()
        {
            string expected = """{"shapeType":"triangle","triangleType":"equilateral"}""";
            EquilateralTriangle? triangle = JsonSerializer.Deserialize<EquilateralTriangle>(expected, _jsonSerializerOptions);
            string actual = JsonSerializer.Serialize(triangle, _jsonSerializerOptions);
            Assert.AreEqual(expected, actual);
        }

        [TestMethod]
        public void Quadrilateral()
        {
            string expected = """{"quadrilateralType":"SimpleQuadrilateral","shapeType":"some quadralateral"}""";
            Quadrilateral? quadrilateral = JsonSerializer.Deserialize<Quadrilateral>(expected, _jsonSerializerOptions);
            string actual = JsonSerializer.Serialize(quadrilateral, _jsonSerializerOptions);
            Assert.AreEqual(expected, actual);
        }

        [TestMethod]
        public void SimpleQuadrilateral()
        {
            string expected = """{"quadrilateralType":"SimpleQuadrilateral","shapeType":"some quadralateral"}""";
            SimpleQuadrilateral? simpleQuadrilateral = JsonSerializer.Deserialize<SimpleQuadrilateral>(expected, _jsonSerializerOptions);
            string simpleQuadrilateralActual = JsonSerializer.Serialize(simpleQuadrilateral, _jsonSerializerOptions);
            Assert.AreEqual(expected, simpleQuadrilateralActual);
        }

        [TestMethod]
        public void ComplexQuadrilateral()
        {
            string complexQuadrilateralExpected = """{"quadrilateralType":"ComplexQuadrilateral","shapeType":"some quadralateral"}""";
            ComplexQuadrilateral? complexQuadrilateral = JsonSerializer.Deserialize<ComplexQuadrilateral>(complexQuadrilateralExpected, _jsonSerializerOptions);
            string complexQuadrilateralActual = JsonSerializer.Serialize(complexQuadrilateral, _jsonSerializerOptions);
            Assert.AreEqual(complexQuadrilateralExpected, complexQuadrilateralActual);
        }

        [TestMethod]
        public void ChildCat()
        {
            string childCatExpected = """{"name":"foo","pet_type":"ChildCat"}""";
            ChildCat? childCat = JsonSerializer.Deserialize<ChildCat>(childCatExpected, _jsonSerializerOptions);
            string childCatActual = JsonSerializer.Serialize(childCat, _jsonSerializerOptions);
            Assert.AreEqual(childCatExpected, childCatActual);
        }

        [TestMethod]
        public void Cat()
        {
            string catExpected = """{"className":"Cat","color":"black","declawed":true}""";
            Cat? cat = JsonSerializer.Deserialize<Cat>(catExpected, _jsonSerializerOptions);
            string childCatActual = JsonSerializer.Serialize(cat, _jsonSerializerOptions);
            Assert.AreEqual(catExpected, childCatActual);
        }

        [TestMethod]
        public void Dog()
        {
            string dogExpected = """{"className":"Dog","breed":"Maltese"}""";
            Dog? dog = JsonSerializer.Deserialize<Dog>(dogExpected, _jsonSerializerOptions);
            string childCatActual = JsonSerializer.Serialize(dog, _jsonSerializerOptions);
            Assert.AreEqual(dogExpected, childCatActual);
        }

        [TestMethod]
        public void Whale()
        {
            string expected = """{"className":"whale","hasBaleen":true,"hasTeeth":true}""";
            Whale? whale = JsonSerializer.Deserialize<Whale>(expected, _jsonSerializerOptions);
            string result = JsonSerializer.Serialize(whale, _jsonSerializerOptions);
            Assert.AreEqual(expected, result);
        }

        [TestMethod]
        public void Mammal()
        {
            string expected = """{"className":"whale","hasBaleen":true,"hasTeeth":true}""";
            Mammal? mammal = JsonSerializer.Deserialize<Mammal>(expected, _jsonSerializerOptions);
            string result = JsonSerializer.Serialize(mammal, _jsonSerializerOptions);
            Assert.AreEqual(expected, result);
        }

        [TestMethod]
        public void ParentPet()
        {
            string expected = """{"pet_type":"ParentPet"}""";
            ParentPet? parentPet = JsonSerializer.Deserialize<ParentPet>(expected, _jsonSerializerOptions);
            string result = JsonSerializer.Serialize(parentPet, _jsonSerializerOptions);
            Assert.AreEqual(expected, result);
        }

        [TestMethod]
        public void Animal()
        {
            string expected = """{"className":"Dog","breed":"maltese","color":"white"}""";
            Animal? animal = JsonSerializer.Deserialize<Animal>(expected, _jsonSerializerOptions);
            Assert.IsInstanceOfType<Dog>(animal);
            string result = JsonSerializer.Serialize(animal, _jsonSerializerOptions);
            Assert.AreEqual(expected, result);
        }
    }
}