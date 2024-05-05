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
            Apple apple = new("#000000", "cultivar", "origin");
            string appleJson = JsonSerializer.Serialize(apple, _jsonSerializerOptions);
            Apple? apple2 = JsonSerializer.Deserialize<Apple>(appleJson, _jsonSerializerOptions);
            Assert.IsTrue(apple2 != null && apple.Cultivar != null && apple.Cultivar.Equals(apple2.Cultivar) && apple.Origin != null && apple.Origin.Equals(apple2.Origin));
        }

        [TestMethod]
        public void Pig()
        {
           BasquePig basquePig = new();
           Pig pig = new(basquePig);
           string pigJson = JsonSerializer.Serialize(pig, _jsonSerializerOptions);
           Pig? pig2 = JsonSerializer.Deserialize<Pig>(pigJson, _jsonSerializerOptions);
            Assert.IsTrue(
                pig.DanishPig == null &&
                pig.BasquePig != null &&
                pig2 != null &&
                pig2.BasquePig != null &&
               pig2.DanishPig == null);
        }

        [TestMethod]
        public void DanishPig()
        {
            DanishPig danishPig = new();
            string danishPigJson = JsonSerializer.Serialize(danishPig, _jsonSerializerOptions);
            DanishPig? danishPig2 = JsonSerializer.Deserialize<DanishPig>(danishPigJson, _jsonSerializerOptions);
            Assert.IsTrue(danishPig2 != null);
        }

        [TestMethod]
        public void GmFruit()
        {
            Apple apple = new("#000000", "cultivar", "origin");
            Banana banana = new(10);
            GmFruit gmFruit = new(apple, banana, "yellow");
            string gmFruitJson = JsonSerializer.Serialize(gmFruit, _jsonSerializerOptions);
            GmFruit? gmFruit2 = JsonSerializer.Deserialize<GmFruit>(gmFruitJson, _jsonSerializerOptions);
            Assert.IsTrue(
                gmFruit.Apple != null &&
                gmFruit.Banana != null &&
                gmFruit2 != null &&
                gmFruit2.Apple != null &&
                gmFruit2.Banana != null &&
                gmFruit2.Apple.Cultivar != null &&
                gmFruit2.Apple.Cultivar.Equals(gmFruit.Apple.Cultivar) &&
                gmFruit2.Apple.Origin != null &&
                gmFruit2.Apple.Origin.Equals(gmFruit.Apple.Origin) &&
                gmFruit2.Banana.LengthCm.Equals(gmFruit.Banana.LengthCm));

            // TODO: assert the properties from Banana and GmFruit are in additionalProperties
        }

        [TestMethod]
        public void EquilateralTriangle()
        {
            EquilateralTriangle equilateralTriangle = new("equilateral");
            string equilateralTriangleJson = JsonSerializer.Serialize(equilateralTriangle, _jsonSerializerOptions);
            EquilateralTriangle? equilateralTriangle2 = JsonSerializer.Deserialize<EquilateralTriangle>(equilateralTriangleJson, _jsonSerializerOptions);
            Assert.IsTrue(equilateralTriangle2 != null && equilateralTriangle.ShapeType.Equals(equilateralTriangle2.ShapeType));
        }

        [TestMethod]
        public void Quadrilateral()
        {
           ComplexQuadrilateral complexQuadrilateral = new("shapeType");
           Quadrilateral quadrilateral = new(complexQuadrilateral);
           string quadrilateralJson = JsonSerializer.Serialize(quadrilateral, _jsonSerializerOptions);
           Quadrilateral? quadrilateral2 = JsonSerializer.Deserialize<Quadrilateral>(quadrilateralJson, _jsonSerializerOptions);
           Assert.IsTrue(
               quadrilateral.ComplexQuadrilateral != null &&
               quadrilateral2 != null &&
               quadrilateral2.SimpleQuadrilateral == null &&
               quadrilateral2.ComplexQuadrilateral != null &&
               quadrilateral2.ComplexQuadrilateral.ShapeType.Equals(quadrilateral.ComplexQuadrilateral.ShapeType));
        }

        [TestMethod]
        public void ChildCat()
        {
            ChildCat childCat = new("some name");
            string childCatJson = JsonSerializer.Serialize(childCat, _jsonSerializerOptions);
            ChildCat? childCat2 = JsonSerializer.Deserialize<ChildCat>(childCatJson, _jsonSerializerOptions);
            Assert.IsTrue(childCat2 != null && childCat.Name != null && childCat.Name.Equals(childCat2.Name));
        }

        [TestMethod]
        public void Cat()
        {
            Cat cat = new("black", false);
            string catJson = JsonSerializer.Serialize(cat, _jsonSerializerOptions);
            Cat? cat2 = JsonSerializer.Deserialize<Cat>(catJson, _jsonSerializerOptions);
            Assert.IsTrue(cat2 != null && cat.Declawed.Equals(cat2.Declawed) && cat.Color != null && cat.Color.Equals(cat2.Color));
        }

        [TestMethod]
        public void OneOfWhale()
        {
            string expected = """{"className":"whale","hasBaleen":true,"hasTeeth":true}""";
            Whale? whale = JsonSerializer.Deserialize<Whale>(expected, _jsonSerializerOptions);
            string result = JsonSerializer.Serialize(whale, _jsonSerializerOptions);
            Assert.AreEqual(expected, result);
        }

        [TestMethod]
        public void OneOfMammal()
        {
            string expected = """{"className":"whale","hasBaleen":true,"hasTeeth":true}""";
            Mammal? mammal = JsonSerializer.Deserialize<Mammal>(expected, _jsonSerializerOptions);
            string result = JsonSerializer.Serialize(mammal, _jsonSerializerOptions);
            Assert.AreEqual(expected, result);
        }

        [TestMethod]
        public void CatFromJson()
        {
            string expected = """{"className":"Cat","color":"black","declawed":true}""";
            Cat? cat = JsonSerializer.Deserialize<Cat>(expected, _jsonSerializerOptions);
            string result = JsonSerializer.Serialize(cat, _jsonSerializerOptions);
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
        public void ChildCatFromJson()
        {
            string expected = """{"name":"foo","pet_type":"ChildCat"}""";
            ChildCat? childCat = JsonSerializer.Deserialize<ChildCat>(expected, _jsonSerializerOptions);
            string result = JsonSerializer.Serialize(childCat, _jsonSerializerOptions);
            Assert.AreEqual(expected, result);
        }

        [TestMethod]
        public void GmFruitFromJson()
        {
            string expected = """{"color_code":"red","cultivar":"cultivar","origin":"origin","color":"red"}""";
            GmFruit? gmFruit = JsonSerializer.Deserialize<GmFruit>(expected, _jsonSerializerOptions);
            string result = JsonSerializer.Serialize(gmFruit, _jsonSerializerOptions);
            Assert.AreEqual(expected, result);
        }

        [TestMethod]
        public void GmFruitFromJsonTwoTypes()
        {
            string expected = """{"color_code":"red","cultivar":"cultivar","origin":"origin","lengthCm":1,"color":"red"}""";
            GmFruit? gmFruit = JsonSerializer.Deserialize<GmFruit>(expected, _jsonSerializerOptions);
            string result = JsonSerializer.Serialize(gmFruit, _jsonSerializerOptions);
            Assert.AreEqual(expected, result);
        }

        [TestMethod]
        public void AppleFromJson()
        {
            string expected = """{"color_code":"red","cultivar":"cultivar","origin":"origin"}""";
            Apple? apple = JsonSerializer.Deserialize<Apple> (expected, _jsonSerializerOptions);
            string result = JsonSerializer.Serialize(apple, _jsonSerializerOptions);
            Assert.AreEqual(expected, result);
        }

        [TestMethod]
        public void AnimalFromJson()
        {
            string expected = """{"className":"Dog","breed":"maltese","color":"white"}""";
            Animal? animal = JsonSerializer.Deserialize<Animal>(expected, _jsonSerializerOptions);
            Assert.IsInstanceOfType<Dog>(animal);
            string result = JsonSerializer.Serialize(animal, _jsonSerializerOptions);
            Assert.AreEqual(expected, result);
        }
    }
}