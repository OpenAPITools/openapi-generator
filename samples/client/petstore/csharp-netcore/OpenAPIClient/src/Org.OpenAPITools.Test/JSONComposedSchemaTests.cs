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

            Fruit f3 = Fruit.FromJson("{\"lengthCm\":98}");
            Assert.IsType<Banana>(f3.ActualInstance);

            Fruit f4 = Fruit.FromJson("{\"origin\":\"Japan\"}");
            Assert.IsType<Apple>(f4.ActualInstance);

            // test custom deserializer
            Fruit f5 = JsonConvert.DeserializeObject<Fruit>("{\"lengthCm\":98}");
            Assert.IsType<Banana>(f5.ActualInstance);

            // test custom serializer
            Assert.Equal("{\"lengthCm\":98.0}", JsonConvert.SerializeObject(f5));
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
        }
    }
}