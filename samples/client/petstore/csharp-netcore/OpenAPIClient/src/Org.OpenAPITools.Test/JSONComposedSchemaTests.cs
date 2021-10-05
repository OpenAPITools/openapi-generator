using System;
using System.IO;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Reflection;
using RestSharp;
using Xunit;

using Org.OpenAPITools.Client;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Model;
using Newtonsoft.Json;

namespace Org.OpenAPITools.Test
{
    /// <summary>
    /// Class for testing Configuration
    /// </summary>
    public class JSONComposedSchemaTests
    {
        public JSONComposedSchemaTests()
        {
        }

        /// <summary>
        /// Test GetServerUrl
        /// </summary>
        [Fact]
        public void TestOneOfSchemaAdditionalProperties()
        {
            // TODO
        }

        /// <summary>
        /// Test GetServerUrl
        /// </summary>
        [Fact]
        public void TestOneOfSchemaWithDiscriminator()
        {
            // Mammal can be one of whale, pig and zebra.
            // pig has sub-classes.

            String str = "{ \"className\": \"whale\", \"hasBaleen\": true, \"hasTeeth\": false }";
            Mammal m = Mammal.FromJson(str);
            Assert.NotNull(m);
            Assert.IsType<Whale>(m.ActualInstance);

            String str2 = "{ \"className\": \"zebra\", \"type\": \"plains\" }";
            Mammal m2 = Mammal.FromJson(str2);
            Assert.NotNull(m2);
            Assert.IsType<Zebra>(m2.ActualInstance);
        }

        /// <summary>
        /// Test Fruit
        /// </summary>
        [Fact]
        public void TestFruit()
        {
            Apple a = new Apple();
            a.Origin = "Japan";

            Banana b = new Banana();
            b.LengthCm = 13;

            Tag t = new Tag();
            t.Id = 12;
            t.Name = "Something";

            Fruit f1 = new Fruit(a);
            Fruit f2 = new Fruit(b);

            f1.ActualInstance = b;
            f2.ActualInstance = a;

            Assert.Equal(13, f1.GetBanana().LengthCm);
            Assert.Equal("Japan", f2.GetApple().Origin);

            Assert.Throws<System.ArgumentException>(() => f1.ActualInstance = t);

            Assert.Equal("{\"lengthCm\":13.0}", f1.ToJson());
            Assert.Equal("{\"origin\":\"Japan\"}", f2.ToJson());
        }

        /// <summary>
        /// Test Fruit with JSON payload matching both Apple and Banana
        /// </summary>
        [Fact]
        public void TestFruitWithPayloadMatchingMoreThanOne()
        {
            // more than 1 match
            Assert.Throws<InvalidDataException>(() => Fruit.FromJson("{\"origin\":\"Japan\"}"));
            Assert.Throws<InvalidDataException>(() => Fruit.FromJson("{\"lengthCm\":98}"));
        }

        /// <summary>
        /// Apple Properties tests
        /// </summary>
        [Fact]
        public void TestAppleProperties()
        {
            Assert.NotNull(typeof(Apple).GetProperty("Origin"));
            Assert.NotNull(typeof(Apple).GetProperty("AdditionalProperties"));
        }

        /// <summary>
        /// Apple tests
        /// </summary>
        [Fact]
        public void TestApple()
        {
            Apple a = JsonConvert.DeserializeObject<Apple>("{\"origin\":\"Japan\"}", Fruit.AdditionalPropertiesSerializerSettings);
            Assert.Equal("{\"origin\":\"Japan\"}", JsonConvert.SerializeObject(a));
        }

        /// <summary>
        /// Banana tests
        /// </summary>
        [Fact]
        public void TestBanana()
        {
            Banana a = JsonConvert.DeserializeObject<Banana>("{\"lengthCm\":98}", Fruit.AdditionalPropertiesSerializerSettings);
            Assert.Equal("{\"lengthCm\":98.0}", JsonConvert.SerializeObject(a));
        }

        /// <summary>
        /// ReadOnly property tests
        /// </summary>
        [Fact]
        public void TestReadOnlyFruit()
        {
            ReadOnlyFirst r = JsonConvert.DeserializeObject<ReadOnlyFirst>("{\"baz\":\"from json gaz\",\"bar\":\"from json bar\"}");
            Assert.Equal("from json bar", r.Bar);
            Assert.Equal("{\"baz\":\"from json gaz\"}", JsonConvert.SerializeObject(r));
        }

        /// <summary>
        /// Cat property tests
        /// </summary>
        [Fact]
        public void TestCat()
        {
            // test to ensure both Cat and Animal (parent) can have "AdditionalProperties", which result in warnings
            Cat c = JsonConvert.DeserializeObject<Cat>("{\"className\":\"cat\",\"bar\":\"from json bar\"}");
            Assert.Equal("from json bar", c.AdditionalProperties["bar"]);

            Cat c2 = new Cat();
            c2.Color = "red";
            c2.Declawed = false;
            Assert.Equal("{\"declawed\":false,\"className\":\"Cat\",\"color\":\"red\"}", JsonConvert.SerializeObject(c2));
        }

        /// <summary>
        /// Test additonal properties
        /// </summary>
        [Fact]
        public void TestAdditionalProperties()
        {
            Foo f = new Foo();
            Assert.NotNull(f.GetType().GetProperty("AdditionalProperties"));
            Assert.Null(f.GetType().GetProperty("unknown_property"));
        }

        /// <summary>
        /// Test OuterEnumInteger
        /// </summary>
        [Fact]
        public void OuterEnumIntegerInstanceTest()
        {
            OuterEnumInteger instance = OuterEnumInteger.NUMBER_1;
            Assert.Equal(1, (int)instance);
        }

        /// <summary>
        /// Test inner enum integer
        /// </summary>
        [Fact]
        public void InnerEnumIntegerInstanceTest()
        {
            EnumTest enumTest = new EnumTest();
            enumTest.EnumIntegerOnly = EnumTest.EnumIntegerOnlyEnum.NUMBER_2;
            enumTest.EnumInteger = EnumTest.EnumIntegerEnum.NUMBER_MINUS_1;
            Assert.Equal("{\"enum_integer\":-1,\"enum_integer_only\":2,\"outerEnum\":null}", JsonConvert.SerializeObject(enumTest));
        }
    }
}
