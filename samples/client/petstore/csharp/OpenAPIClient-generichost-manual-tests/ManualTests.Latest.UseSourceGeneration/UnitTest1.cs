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
        }

        [TestMethod]
        public void Category()
        {
            CategorySerializationContext serializationContext = _host.Services.GetRequiredService<CategorySerializationContext>();
            CategoryDeserializationContext deserializationContext = _host.Services.GetRequiredService<CategoryDeserializationContext>();

            Category category = new(1, "test");
            string categoryJson = JsonSerializer.Serialize(category, serializationContext.Category);
            Category? category2 = JsonSerializer.Deserialize(categoryJson, deserializationContext.Category);
            Assert.AreEqual(category.Id, category2?.Id);
            Assert.AreEqual(category.Name, category2?.Name);
        }

        [TestMethod]
        public void Apple()
        {
            AppleSerializationContext serializationContext = _host.Services.GetRequiredService<AppleSerializationContext>();
            AppleDeserializationContext deserializationContext = _host.Services.GetRequiredService<AppleDeserializationContext>();

            Apple apple = new("#000000", "cultivar", "origin");
            string appleJson = JsonSerializer.Serialize(apple, serializationContext.Apple);
            Apple? apple2 = JsonSerializer.Deserialize(appleJson, deserializationContext.Apple);
            Assert.IsTrue(apple2 != null && apple.Cultivar.Equals(apple2.Cultivar) && apple.Origin.Equals(apple2.Origin));
        }

        [TestMethod]
        public void Pig()
        {
            PigSerializationContext serializationContext = _host.Services.GetRequiredService<PigSerializationContext>();
            PigDeserializationContext deserializationContext = _host.Services.GetRequiredService<PigDeserializationContext>();

            BasquePig basquePig = new("BasquePig");
            Pig pig = new(basquePig, "BasquePig");
            string pigJson = JsonSerializer.Serialize(pig, serializationContext.Pig);
            Pig? pig2 = JsonSerializer.Deserialize<Pig>(pigJson, deserializationContext.Pig);
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
            DanishPigSerializationContext serializationContext = _host.Services.GetRequiredService<DanishPigSerializationContext>();
            DanishPigDeserializationContext deserializationContext = _host.Services.GetRequiredService<DanishPigDeserializationContext>();

            DanishPig danishPig = new("danishPig");
            string danishPigJson = JsonSerializer.Serialize(danishPig, serializationContext.DanishPig);
            DanishPig? danishPig2 = JsonSerializer.Deserialize(danishPigJson, deserializationContext.DanishPig);
            Assert.IsTrue(danishPig2 != null && danishPig.ClassName.Equals(danishPig2.ClassName));
        }

        [TestMethod]
        public void GmFruit()
        {
            GmFruitSerializationContext serializationContext = _host.Services.GetRequiredService<GmFruitSerializationContext>();
            GmFruitDeserializationContext deserializationContext = _host.Services.GetRequiredService<GmFruitDeserializationContext>();

            Apple apple = new("#000000", "cultivar", "origin");
            Banana banana = new(10);
            GmFruit gmFruit = new(apple, banana, "yellow");
            string gmFruitJson = JsonSerializer.Serialize(gmFruit, serializationContext.GmFruit);
            GmFruit? gmFruit2 = JsonSerializer.Deserialize<GmFruit>(gmFruitJson, deserializationContext.GmFruit);
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
            EquilateralTriangleSerializationContext serializationContext = _host.Services.GetRequiredService<EquilateralTriangleSerializationContext>();
            EquilateralTriangleDeserializationContext deserializationContext = _host.Services.GetRequiredService<EquilateralTriangleDeserializationContext>();

            EquilateralTriangle equilateralTriangle = new("triangle", "equilateral");
            string equilateralTriangleJson = JsonSerializer.Serialize(equilateralTriangle, serializationContext.EquilateralTriangle);
            EquilateralTriangle? equilateralTriangle2 = JsonSerializer.Deserialize(equilateralTriangleJson, deserializationContext.EquilateralTriangle);
            Assert.IsTrue(equilateralTriangle2 != null && equilateralTriangle.TriangleType.Equals(equilateralTriangle2.TriangleType) && equilateralTriangle.ShapeType.Equals(equilateralTriangle2.ShapeType));
        }

        [TestMethod]
        public void Quadrilateral()
        {
            QuadrilateralSerializationContext serializationContext = _host.Services.GetRequiredService<QuadrilateralSerializationContext>();
            QuadrilateralDeserializationContext deserializationContext = _host.Services.GetRequiredService<QuadrilateralDeserializationContext>();

            ComplexQuadrilateral complexQuadrilateral = new("ComplexQuadrilateral", "shapeType");
            Quadrilateral quadrilateral = new(complexQuadrilateral, "ComplexQuadrilateral");
            string quadrilateralJson = JsonSerializer.Serialize(quadrilateral, serializationContext.Quadrilateral);
            Quadrilateral? quadrilateral2 = JsonSerializer.Deserialize(quadrilateralJson, deserializationContext.Quadrilateral);
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
            ChildCatSerializationContext serializationContext = _host.Services.GetRequiredService<ChildCatSerializationContext>();
            ChildCatDeserializationContext deserializationContext = _host.Services.GetRequiredService<ChildCatDeserializationContext>();

            ChildCat childCat = new("some name", ChildCat.PetTypeEnum.ChildCat);
            string childCatJson = JsonSerializer.Serialize(childCat, serializationContext.ChildCat);
            ChildCat? childCat2 = JsonSerializer.Deserialize(childCatJson, deserializationContext.ChildCat);
            Assert.IsTrue(childCat2 != null && childCat.PetType.Equals(childCat2.PetType) && childCat.Name.Equals(childCat2.Name));
        }

        [TestMethod]
        public void Cat()
        {
            CatSerializationContext serializationContext = _host.Services.GetRequiredService<CatSerializationContext>();
            CatDeserializationContext deserializationContext = _host.Services.GetRequiredService<CatDeserializationContext>();

            Cat cat = new("cat", false, "black"); // TODO: where is the address property?
            string catJson = JsonSerializer.Serialize(cat, serializationContext.Cat);
            Cat? cat2 = JsonSerializer.Deserialize(catJson, deserializationContext.Cat);
            Assert.IsTrue(cat2 != null && cat.Declawed.Equals(cat2.Declawed) && cat.ClassName.Equals(cat2.ClassName) && cat.Color.Equals(cat2.Color)); // TODO: add the address property
        }
    }
}