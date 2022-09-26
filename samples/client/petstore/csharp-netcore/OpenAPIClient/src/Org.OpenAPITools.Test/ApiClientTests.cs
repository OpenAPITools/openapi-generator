using System;
using System.IO;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Reflection;
using Newtonsoft.Json;
using Newtonsoft.Json.Serialization;
using RestSharp;
using Xunit;

using Org.OpenAPITools.Client;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Model;

namespace Org.OpenAPITools.Test
{
    /// <summary>
    /// Class for testing ApiClient
    /// </summary>
    public class ApiClientTests
    {
        public ApiClientTests()
        {
        }

        /// <summary>
        /// Test GetSerializerSettingsTest
        /// </summary>
        [Fact]
        public void GetSerializerSettingsTest()
        {
            ApiClient apiClient = new ApiClient();

            var serializerSettingsPropertyInfo = typeof(ApiClient).GetProperty(nameof(ApiClient.SerializerSettings));

            // Validate that we can the set the SerializerSettings (public visibility)
            Assert.NotNull(serializerSettingsPropertyInfo?.GetSetMethod());

            // Validate default serializer settings
            Assert.NotNull(apiClient.SerializerSettings);
            Assert.Equal(ConstructorHandling.AllowNonPublicDefaultConstructor, apiClient.SerializerSettings.ConstructorHandling);
            Assert.False(((DefaultContractResolver)apiClient.SerializerSettings.ContractResolver).NamingStrategy.OverrideSpecifiedNames);
        }
    }
}
