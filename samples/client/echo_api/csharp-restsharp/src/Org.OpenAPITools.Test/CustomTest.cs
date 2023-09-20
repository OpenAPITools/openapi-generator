using Newtonsoft.Json.Linq;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Model;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Xml;
using Xunit;

namespace Org.OpenAPITools.Test
{
    public class CustomTest
    {
        private QueryApi api = new QueryApi();
        private BodyApi bodyApi = new BodyApi();

        [Fact]
        public void TestEchoBodyPet()
        {
            Pet queryObject = new Pet(12345L, "Hello World", new Category(987L, "new category"), new List<string> { "http://a.com", "http://b.com" });
            Pet p = bodyApi.TestEchoBodyPet(queryObject);
            Assert.NotNull(p);
            Assert.Equal("Hello World", p.Name);
            Assert.Equal(12345L, p.Id);

            // response is empty body
            Pet p2 = bodyApi.TestEchoBodyPet(null);
            Assert.Null(p2);
        }

        /**
         * Test query parameter(s)
         * <p>
         * Test query parameter(s)
         *
         * @throws ApiException if the Api call fails
         */
        [Fact]
        public void TestQueryStyleFormExplodeTrueObjectTest()
        {
            Pet queryObject = new Pet(12345L, "Hello World", new Category(987L, "new category"), new List<string> { "http://a.com", "http://b.com" });
            String response = api.TestQueryStyleFormExplodeTrueObject(queryObject);
            EchoServerResponseParser p = new EchoServerResponseParser(response);
            Assert.Equal("/query/style_form/explode_true/object?query_object=class%20Pet%20%7b%0a%20%20Id%3a%2012345%0a%20%20Name%3a%20Hello%20World%0a%20%20Category%3a%20class%20Category%20%7b%0a%20%20Id%3a%20987%0a%20%20Name%3a%20new%20category%0a%7d%0a%0a%20%20PhotoUrls%3a%20System.Collections.Generic.List%601%5bSystem.String%5d%0a%20%20Tags%3a%20%0a%20%20Status%3a%20%0a%7d%0a", p.path);
        }

        [Fact]
        public void testQueryStyleDeepObjectExplodeTrueObjectTest()
        {
            Pet queryObject = new Pet(12345L, "Hello World", new Category(987L, "new category"), new List<string> { "http://a.com", "http://b.com" });
            String response = api.TestQueryStyleDeepObjectExplodeTrueObject(queryObject);
            EchoServerResponseParser p = new EchoServerResponseParser(response);
            Assert.Equal("/query/style_deepObject/explode_true/object?queryObject%5bid%5d=12345&queryObject%5bname%5d=Hello%20World&queryObject%5bcategory%5d=class%20Category%20%7b%0a%20%20Id%3a%20987%0a%20%20Name%3a%20new%20category%0a%7d%0a&queryObject%5bphotoUrls%5d=http%3a%2f%2fa.com%2chttp%3a%2f%2fb.com", p.path);
        }


        [Fact]
        public void testQueryStyleDeepObjectExplodeTrueObjectAsyncTest()
        {
            Pet queryObject = new Pet(12345L, "Hello World", new Category(987L, "new category"), new List<string> { "http://a.com", "http://b.com" });
            Task<String> responseTask = api.TestQueryStyleDeepObjectExplodeTrueObjectAsync(queryObject);
            EchoServerResponseParser p = new EchoServerResponseParser(responseTask.Result);
            Assert.Equal("/query/style_deepObject/explode_true/object?queryObject%5bid%5d=12345&queryObject%5bname%5d=Hello%20World&queryObject%5bcategory%5d=class%20Category%20%7b%0a%20%20Id%3a%20987%0a%20%20Name%3a%20new%20category%0a%7d%0a&queryObject%5bphotoUrls%5d=http%3a%2f%2fa.com%2chttp%3a%2f%2fb.com", p.path);
        }
    }
}
