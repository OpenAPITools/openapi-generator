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

namespace Org.OpenAPITools.Test
{
    /// <summary>
    /// Class for testing Configuration
    /// </summary>
    public class ConfigurationTests
    {
        public ConfigurationTests()
        {
        }

        /// <summary>
        /// Test GetServerUrl
        /// </summary>
        [Fact]
        public void GetServerUrlTest()
        {
            Configuration c = new Configuration();
            // no variable (null) provided
            Assert.Equal("https://localhost:8080/v2", c.GetServerUrl(1, null));
            // no variable (empty dictionary) provided
            Assert.Equal("https://localhost:8080/v2", c.GetServerUrl(1, new Dictionary<string, string>()));

            Assert.Equal("https://localhost:8080/v1", c.GetServerUrl(1, new Dictionary<string, string>() { { "version", "v1" } }));

            Assert.Throws<InvalidOperationException>(() => c.GetServerUrl(1, new Dictionary<string, string>() { { "version", "v3" } }));

            // test the first server (index 0)
            Assert.Equal("http://petstore.swagger.io:80/v2", c.GetServerUrl(0));

        }
    }
}
