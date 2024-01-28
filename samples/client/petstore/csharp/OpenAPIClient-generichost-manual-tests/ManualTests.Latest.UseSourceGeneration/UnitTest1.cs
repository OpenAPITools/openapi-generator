using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using System.Text.Json;
using UseSourceGeneration.Client;
using UseSourceGeneration.Model;
using UseSourceGeneration.Extensions;

namespace ManualTests.Latest.UseSourceGeneration;

[TestClass]
public class UnitTest1
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
                ApiKeyToken apiKeyToken = new(apiKeyTokenValue, timeout: TimeSpan.FromSeconds(1));
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
        public void Category()
        {
            Category category = new(1, "test");
            string categoryJson = JsonSerializer.Serialize(category, _jsonSerializerOptions);
            Category? category2 = JsonSerializer.Deserialize<Category>(categoryJson, _jsonSerializerOptions);
            Assert.AreEqual(category.Id, category2?.Id);
            Assert.AreEqual(category.Name, category2?.Name);
        }

        [TestMethod]
        public void Apple()
        {
            Apple apple = new("#000000", "cultivar", "origin");
            string appleJson = JsonSerializer.Serialize(apple, _jsonSerializerOptions);
            Apple? apple2 = JsonSerializer.Deserialize<Apple>(appleJson, _jsonSerializerOptions);
            Assert.IsTrue(apple2 != null && apple.Cultivar.Equals(apple2.Cultivar) && apple.Origin.Equals(apple2.Origin));
        }

        [TestMethod]
        public void Pig()
        {
            BasquePig basquePig = new("BasquePig");
            Pig pig = new(basquePig, "BasquePig");
            string pigJson = JsonSerializer.Serialize(pig, _jsonSerializerOptions);
            Pig? pig2 = JsonSerializer.Deserialize<Pig>(pigJson, _jsonSerializerOptions);
            Assert.IsTrue(
                pig.DanishPig == null &&
                pig.BasquePig != null &&
                pig2 != null &&
                pig2.BasquePig != null &&
                pig2.DanishPig == null &&
                pig2.BasquePig.ClassName.Equals(pig.BasquePig.ClassName));
        }

        [TestMethod]
        public void DanishPig()
        {
            DanishPig danishPig = new("danishPig");
            string danishPigJson = JsonSerializer.Serialize(danishPig, _jsonSerializerOptions);
            DanishPig? danishPig2 = JsonSerializer.Deserialize<DanishPig>(danishPigJson, _jsonSerializerOptions);
            Assert.IsTrue(danishPig2 != null && danishPig.ClassName.Equals(danishPig2.ClassName));
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
                gmFruit2.Apple.Cultivar.Equals(gmFruit.Apple.Cultivar) &&
                gmFruit2.Apple.Origin.Equals(gmFruit.Apple.Origin) &&
                gmFruit2.Banana.LengthCm.Equals(gmFruit.Banana.LengthCm));

            Apple? apple2 = JsonSerializer.Deserialize<Apple>(gmFruitJson);
            Assert.IsTrue(apple2 != null && apple.Cultivar == apple2.Cultivar && apple.Origin == apple2.Origin);
            // TODO: assert the the properties from Banana and GmFruit are in additionalProperties
        }

        [TestMethod]
        public void EquilateralTriangle()
        {
            EquilateralTriangle equilateralTriangle = new("triangle", "equilateral");
            string equilateralTriangleJson = JsonSerializer.Serialize(equilateralTriangle, _jsonSerializerOptions);
            EquilateralTriangle? equilateralTriangle2 = JsonSerializer.Deserialize<EquilateralTriangle>(equilateralTriangleJson, _jsonSerializerOptions);
            Assert.IsTrue(equilateralTriangle2 != null && equilateralTriangle.TriangleType.Equals(equilateralTriangle2.TriangleType) && equilateralTriangle.ShapeType.Equals(equilateralTriangle2.ShapeType));
        }

        [TestMethod]
        public void Quadrilateral()
        {
            ComplexQuadrilateral complexQuadrilateral = new("ComplexQuadrilateral", "shapeType");
            Quadrilateral quadrilateral = new(complexQuadrilateral, "ComplexQuadrilateral");
            string quadrilateralJson = JsonSerializer.Serialize(quadrilateral, _jsonSerializerOptions);
            Quadrilateral? quadrilateral2 = JsonSerializer.Deserialize<Quadrilateral>(quadrilateralJson, _jsonSerializerOptions);
            Assert.IsTrue(
                quadrilateral.ComplexQuadrilateral != null &&
                quadrilateral2 != null &&
                quadrilateral2.SimpleQuadrilateral == null &&
                quadrilateral2.ComplexQuadrilateral != null &&
                quadrilateral2.ComplexQuadrilateral.QuadrilateralType.Equals(quadrilateral.ComplexQuadrilateral.QuadrilateralType) &&
                quadrilateral2.ComplexQuadrilateral.ShapeType.Equals(quadrilateral.ComplexQuadrilateral.ShapeType));
        }

        [TestMethod]
        public void ChildCatTest()
        {
            ChildCat childCat = new("some name", ChildCat.PetTypeEnum.ChildCat);
            string childCatJson = JsonSerializer.Serialize(childCat, _jsonSerializerOptions);
            ChildCat? childCat2 = JsonSerializer.Deserialize<ChildCat>(childCatJson, _jsonSerializerOptions);
            Assert.IsTrue(childCat2 != null && childCat.PetType.Equals(childCat2.PetType) && childCat.Name.Equals(childCat2.Name));
        }

        [TestMethod]
        public void Cat()
        {
            Cat cat = new("cat", false, "black"); // TODO: where is the address property?
            string catJson = JsonSerializer.Serialize(cat, _jsonSerializerOptions);
            Cat? cat2 = JsonSerializer.Deserialize<Cat>(catJson, _jsonSerializerOptions);
            Assert.IsTrue(cat2 != null && cat.Declawed.Equals(cat2.Declawed) && cat.ClassName.Equals(cat2.ClassName) && cat.Color.Equals(cat2.Color)); // TODO: add the address property
        }
    }
}