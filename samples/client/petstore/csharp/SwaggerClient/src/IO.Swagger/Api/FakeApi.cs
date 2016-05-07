using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using RestSharp;
using IO.Swagger.Client;

namespace IO.Swagger.Api
{
    /// <summary>
    /// Represents a collection of functions to interact with the API endpoints
    /// </summary>
    public interface IFakeApi
    {
        #region Synchronous Operations
        /// <summary>
        /// Fake endpoint for testing various parameters
        /// </summary>
        /// <remarks>
        /// Fake endpoint for testing various parameters
        /// </remarks>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="number">None</param>
        /// <param name="_double">None</param>
        /// <param name="_string">None</param>
        /// <param name="_byte">None</param>
        /// <param name="integer">None (optional)</param>
        /// <param name="int32">None (optional)</param>
        /// <param name="int64">None (optional)</param>
        /// <param name="_float">None (optional)</param>
        /// <param name="binary">None (optional)</param>
        /// <param name="date">None (optional)</param>
        /// <param name="dateTime">None (optional)</param>
        /// <param name="password">None (optional)</param>
        /// <returns></returns>
        void TestEndpointParameters (double? number, double? _double, string _string, byte[] _byte, int? integer = null, int? int32 = null, long? int64 = null, float? _float = null, byte[] binary = null, DateTime? date = null, DateTime? dateTime = null, string password = null);

        /// <summary>
        /// Fake endpoint for testing various parameters
        /// </summary>
        /// <remarks>
        /// Fake endpoint for testing various parameters
        /// </remarks>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="number">None</param>
        /// <param name="_double">None</param>
        /// <param name="_string">None</param>
        /// <param name="_byte">None</param>
        /// <param name="integer">None (optional)</param>
        /// <param name="int32">None (optional)</param>
        /// <param name="int64">None (optional)</param>
        /// <param name="_float">None (optional)</param>
        /// <param name="binary">None (optional)</param>
        /// <param name="date">None (optional)</param>
        /// <param name="dateTime">None (optional)</param>
        /// <param name="password">None (optional)</param>
        /// <returns>ApiResponse of Object(void)</returns>
        ApiResponse<Object> TestEndpointParametersWithHttpInfo (double? number, double? _double, string _string, byte[] _byte, int? integer = null, int? int32 = null, long? int64 = null, float? _float = null, byte[] binary = null, DateTime? date = null, DateTime? dateTime = null, string password = null);
        #endregion Synchronous Operations
        #region Asynchronous Operations
        /// <summary>
        /// Fake endpoint for testing various parameters
        /// </summary>
        /// <remarks>
        /// Fake endpoint for testing various parameters
        /// </remarks>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="number">None</param>
        /// <param name="_double">None</param>
        /// <param name="_string">None</param>
        /// <param name="_byte">None</param>
        /// <param name="integer">None (optional)</param>
        /// <param name="int32">None (optional)</param>
        /// <param name="int64">None (optional)</param>
        /// <param name="_float">None (optional)</param>
        /// <param name="binary">None (optional)</param>
        /// <param name="date">None (optional)</param>
        /// <param name="dateTime">None (optional)</param>
        /// <param name="password">None (optional)</param>
        /// <returns>Task of void</returns>
        System.Threading.Tasks.Task TestEndpointParametersAsync (double? number, double? _double, string _string, byte[] _byte, int? integer = null, int? int32 = null, long? int64 = null, float? _float = null, byte[] binary = null, DateTime? date = null, DateTime? dateTime = null, string password = null);

        /// <summary>
        /// Fake endpoint for testing various parameters
        /// </summary>
        /// <remarks>
        /// Fake endpoint for testing various parameters
        /// </remarks>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="number">None</param>
        /// <param name="_double">None</param>
        /// <param name="_string">None</param>
        /// <param name="_byte">None</param>
        /// <param name="integer">None (optional)</param>
        /// <param name="int32">None (optional)</param>
        /// <param name="int64">None (optional)</param>
        /// <param name="_float">None (optional)</param>
        /// <param name="binary">None (optional)</param>
        /// <param name="date">None (optional)</param>
        /// <param name="dateTime">None (optional)</param>
        /// <param name="password">None (optional)</param>
        /// <returns>Task of ApiResponse</returns>
        System.Threading.Tasks.Task<ApiResponse<Object>> TestEndpointParametersAsyncWithHttpInfo (double? number, double? _double, string _string, byte[] _byte, int? integer = null, int? int32 = null, long? int64 = null, float? _float = null, byte[] binary = null, DateTime? date = null, DateTime? dateTime = null, string password = null);
        #endregion Asynchronous Operations
    }

    /// <summary>
    /// Represents a collection of functions to interact with the API endpoints
    /// </summary>
    public class FakeApi : IFakeApi
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="FakeApi"/> class.
        /// </summary>
        /// <returns></returns>
        public FakeApi(String basePath)
        {
            this.Configuration = new Configuration(new ApiClient(basePath));

            // ensure API client has configuration ready
            if (Configuration.ApiClient.Configuration == null)
            {
                this.Configuration.ApiClient.Configuration = this.Configuration;
            }
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="FakeApi"/> class
        /// using Configuration object
        /// </summary>
        /// <param name="configuration">An instance of Configuration</param>
        /// <returns></returns>
        public FakeApi(Configuration configuration = null)
        {
            if (configuration == null) // use the default one in Configuration
                this.Configuration = Configuration.Default;
            else
                this.Configuration = configuration;

            // ensure API client has configuration ready
            if (Configuration.ApiClient.Configuration == null)
            {
                this.Configuration.ApiClient.Configuration = this.Configuration;
            }
        }

        /// <summary>
        /// Gets the base path of the API client.
        /// </summary>
        /// <value>The base path</value>
        public String GetBasePath()
        {
            return this.Configuration.ApiClient.RestClient.BaseUrl.ToString();
        }

        /// <summary>
        /// Sets the base path of the API client.
        /// </summary>
        /// <value>The base path</value>
        [Obsolete("SetBasePath is deprecated, please do 'Configuraiton.ApiClient = new ApiClient(\"http://new-path\")' instead.")]
        public void SetBasePath(String basePath)
        {
            // do nothing
        }

        /// <summary>
        /// Gets or sets the configuration object
        /// </summary>
        /// <value>An instance of the Configuration</value>
        public Configuration Configuration {get; set;}

        /// <summary>
        /// Gets the default header.
        /// </summary>
        /// <returns>Dictionary of HTTP header</returns>
        [Obsolete("DefaultHeader is deprecated, please use Configuration.DefaultHeader instead.")]
        public Dictionary<String, String> DefaultHeader()
        {
            return this.Configuration.DefaultHeader;
        }

        /// <summary>
        /// Add default header.
        /// </summary>
        /// <param name="key">Header field name.</param>
        /// <param name="value">Header field value.</param>
        /// <returns></returns>
        [Obsolete("AddDefaultHeader is deprecated, please use Configuration.AddDefaultHeader instead.")]
        public void AddDefaultHeader(string key, string value)
        {
            this.Configuration.AddDefaultHeader(key, value);
        }

        /// <summary>
        /// Fake endpoint for testing various parameters Fake endpoint for testing various parameters
        /// </summary>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="number">None</param>
        /// <param name="_double">None</param>
        /// <param name="_string">None</param>
        /// <param name="_byte">None</param>
        /// <param name="integer">None (optional)</param>
        /// <param name="int32">None (optional)</param>
        /// <param name="int64">None (optional)</param>
        /// <param name="_float">None (optional)</param>
        /// <param name="binary">None (optional)</param>
        /// <param name="date">None (optional)</param>
        /// <param name="dateTime">None (optional)</param>
        /// <param name="password">None (optional)</param>
        /// <returns></returns>
        public void TestEndpointParameters (double? number, double? _double, string _string, byte[] _byte, int? integer = null, int? int32 = null, long? int64 = null, float? _float = null, byte[] binary = null, DateTime? date = null, DateTime? dateTime = null, string password = null)
        {
             TestEndpointParametersWithHttpInfo(number, _double, _string, _byte, integer, int32, int64, _float, binary, date, dateTime, password);
        }

        /// <summary>
        /// Fake endpoint for testing various parameters Fake endpoint for testing various parameters
        /// </summary>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="number">None</param>
        /// <param name="_double">None</param>
        /// <param name="_string">None</param>
        /// <param name="_byte">None</param>
        /// <param name="integer">None (optional)</param>
        /// <param name="int32">None (optional)</param>
        /// <param name="int64">None (optional)</param>
        /// <param name="_float">None (optional)</param>
        /// <param name="binary">None (optional)</param>
        /// <param name="date">None (optional)</param>
        /// <param name="dateTime">None (optional)</param>
        /// <param name="password">None (optional)</param>
        /// <returns>ApiResponse of Object(void)</returns>
        public ApiResponse<Object> TestEndpointParametersWithHttpInfo (double? number, double? _double, string _string, byte[] _byte, int? integer = null, int? int32 = null, long? int64 = null, float? _float = null, byte[] binary = null, DateTime? date = null, DateTime? dateTime = null, string password = null)
        {
            // verify the required parameter 'number' is set
            if (number == null)
                throw new ApiException(400, "Missing required parameter 'number' when calling FakeApi->TestEndpointParameters");
            // verify the required parameter '_double' is set
            if (_double == null)
                throw new ApiException(400, "Missing required parameter '_double' when calling FakeApi->TestEndpointParameters");
            // verify the required parameter '_string' is set
            if (_string == null)
                throw new ApiException(400, "Missing required parameter '_string' when calling FakeApi->TestEndpointParameters");
            // verify the required parameter '_byte' is set
            if (_byte == null)
                throw new ApiException(400, "Missing required parameter '_byte' when calling FakeApi->TestEndpointParameters");

            var localVarPath = "/fake";
            var localVarPathParams = new Dictionary<String, String>();
            var localVarQueryParams = new Dictionary<String, String>();
            var localVarHeaderParams = new Dictionary<String, String>(Configuration.DefaultHeader);
            var localVarFormParams = new Dictionary<String, String>();
            var localVarFileParams = new Dictionary<String, FileParameter>();
            Object localVarPostBody = null;

            // to determine the Content-Type header
            String[] localVarHttpContentTypes = new String[] {
            };
            String localVarHttpContentType = Configuration.ApiClient.SelectHeaderContentType(localVarHttpContentTypes);

            // to determine the Accept header
            String[] localVarHttpHeaderAccepts = new String[] {
                "application/xml", 
                "application/json"
            };
            String localVarHttpHeaderAccept = Configuration.ApiClient.SelectHeaderAccept(localVarHttpHeaderAccepts);
            if (localVarHttpHeaderAccept != null)
                localVarHeaderParams.Add("Accept", localVarHttpHeaderAccept);

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            localVarPathParams.Add("format", "json");
            if (integer != null) localVarFormParams.Add("integer", Configuration.ApiClient.ParameterToString(integer)); // form parameter
            if (int32 != null) localVarFormParams.Add("int32", Configuration.ApiClient.ParameterToString(int32)); // form parameter
            if (int64 != null) localVarFormParams.Add("int64", Configuration.ApiClient.ParameterToString(int64)); // form parameter
            if (number != null) localVarFormParams.Add("number", Configuration.ApiClient.ParameterToString(number)); // form parameter
            if (_float != null) localVarFormParams.Add("float", Configuration.ApiClient.ParameterToString(_float)); // form parameter
            if (_double != null) localVarFormParams.Add("double", Configuration.ApiClient.ParameterToString(_double)); // form parameter
            if (_string != null) localVarFormParams.Add("string", Configuration.ApiClient.ParameterToString(_string)); // form parameter
            if (_byte != null) localVarFormParams.Add("byte", Configuration.ApiClient.ParameterToString(_byte)); // form parameter
            if (binary != null) localVarFormParams.Add("binary", Configuration.ApiClient.ParameterToString(binary)); // form parameter
            if (date != null) localVarFormParams.Add("date", Configuration.ApiClient.ParameterToString(date)); // form parameter
            if (dateTime != null) localVarFormParams.Add("dateTime", Configuration.ApiClient.ParameterToString(dateTime)); // form parameter
            if (password != null) localVarFormParams.Add("password", Configuration.ApiClient.ParameterToString(password)); // form parameter


            // make the HTTP request
            IRestResponse localVarResponse = (IRestResponse) Configuration.ApiClient.CallApi(localVarPath,
                Method.POST, localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarFileParams,
                localVarPathParams, localVarHttpContentType);

            int localVarStatusCode = (int) localVarResponse.StatusCode;

            if (localVarStatusCode >= 400)
                throw new ApiException (localVarStatusCode, "Error calling TestEndpointParameters: " + localVarResponse.Content, localVarResponse.Content);
            else if (localVarStatusCode == 0)
                throw new ApiException (localVarStatusCode, "Error calling TestEndpointParameters: " + localVarResponse.ErrorMessage, localVarResponse.ErrorMessage);

            
            return new ApiResponse<Object>(localVarStatusCode,
                localVarResponse.Headers.ToDictionary(x => x.Name, x => x.Value.ToString()),
                null);
        }

        /// <summary>
        /// Fake endpoint for testing various parameters Fake endpoint for testing various parameters
        /// </summary>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="number">None</param>
        /// <param name="_double">None</param>
        /// <param name="_string">None</param>
        /// <param name="_byte">None</param>
        /// <param name="integer">None (optional)</param>
        /// <param name="int32">None (optional)</param>
        /// <param name="int64">None (optional)</param>
        /// <param name="_float">None (optional)</param>
        /// <param name="binary">None (optional)</param>
        /// <param name="date">None (optional)</param>
        /// <param name="dateTime">None (optional)</param>
        /// <param name="password">None (optional)</param>
        /// <returns>Task of void</returns>
        public async System.Threading.Tasks.Task TestEndpointParametersAsync (double? number, double? _double, string _string, byte[] _byte, int? integer = null, int? int32 = null, long? int64 = null, float? _float = null, byte[] binary = null, DateTime? date = null, DateTime? dateTime = null, string password = null)
        {
             await TestEndpointParametersAsyncWithHttpInfo(number, _double, _string, _byte, integer, int32, int64, _float, binary, date, dateTime, password);

        }

        /// <summary>
        /// Fake endpoint for testing various parameters Fake endpoint for testing various parameters
        /// </summary>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="number">None</param>
        /// <param name="_double">None</param>
        /// <param name="_string">None</param>
        /// <param name="_byte">None</param>
        /// <param name="integer">None (optional)</param>
        /// <param name="int32">None (optional)</param>
        /// <param name="int64">None (optional)</param>
        /// <param name="_float">None (optional)</param>
        /// <param name="binary">None (optional)</param>
        /// <param name="date">None (optional)</param>
        /// <param name="dateTime">None (optional)</param>
        /// <param name="password">None (optional)</param>
        /// <returns>Task of ApiResponse</returns>
        public async System.Threading.Tasks.Task<ApiResponse<Object>> TestEndpointParametersAsyncWithHttpInfo (double? number, double? _double, string _string, byte[] _byte, int? integer = null, int? int32 = null, long? int64 = null, float? _float = null, byte[] binary = null, DateTime? date = null, DateTime? dateTime = null, string password = null)
        {
            // verify the required parameter 'number' is set
            if (number == null)
                throw new ApiException(400, "Missing required parameter 'number' when calling FakeApi->TestEndpointParameters");
            // verify the required parameter '_double' is set
            if (_double == null)
                throw new ApiException(400, "Missing required parameter '_double' when calling FakeApi->TestEndpointParameters");
            // verify the required parameter '_string' is set
            if (_string == null)
                throw new ApiException(400, "Missing required parameter '_string' when calling FakeApi->TestEndpointParameters");
            // verify the required parameter '_byte' is set
            if (_byte == null)
                throw new ApiException(400, "Missing required parameter '_byte' when calling FakeApi->TestEndpointParameters");

            var localVarPath = "/fake";
            var localVarPathParams = new Dictionary<String, String>();
            var localVarQueryParams = new Dictionary<String, String>();
            var localVarHeaderParams = new Dictionary<String, String>(Configuration.DefaultHeader);
            var localVarFormParams = new Dictionary<String, String>();
            var localVarFileParams = new Dictionary<String, FileParameter>();
            Object localVarPostBody = null;

            // to determine the Content-Type header
            String[] localVarHttpContentTypes = new String[] {
            };
            String localVarHttpContentType = Configuration.ApiClient.SelectHeaderContentType(localVarHttpContentTypes);

            // to determine the Accept header
            String[] localVarHttpHeaderAccepts = new String[] {
                "application/xml", 
                "application/json"
            };
            String localVarHttpHeaderAccept = Configuration.ApiClient.SelectHeaderAccept(localVarHttpHeaderAccepts);
            if (localVarHttpHeaderAccept != null)
                localVarHeaderParams.Add("Accept", localVarHttpHeaderAccept);

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            localVarPathParams.Add("format", "json");
            if (integer != null) localVarFormParams.Add("integer", Configuration.ApiClient.ParameterToString(integer)); // form parameter
            if (int32 != null) localVarFormParams.Add("int32", Configuration.ApiClient.ParameterToString(int32)); // form parameter
            if (int64 != null) localVarFormParams.Add("int64", Configuration.ApiClient.ParameterToString(int64)); // form parameter
            if (number != null) localVarFormParams.Add("number", Configuration.ApiClient.ParameterToString(number)); // form parameter
            if (_float != null) localVarFormParams.Add("float", Configuration.ApiClient.ParameterToString(_float)); // form parameter
            if (_double != null) localVarFormParams.Add("double", Configuration.ApiClient.ParameterToString(_double)); // form parameter
            if (_string != null) localVarFormParams.Add("string", Configuration.ApiClient.ParameterToString(_string)); // form parameter
            if (_byte != null) localVarFormParams.Add("byte", Configuration.ApiClient.ParameterToString(_byte)); // form parameter
            if (binary != null) localVarFormParams.Add("binary", Configuration.ApiClient.ParameterToString(binary)); // form parameter
            if (date != null) localVarFormParams.Add("date", Configuration.ApiClient.ParameterToString(date)); // form parameter
            if (dateTime != null) localVarFormParams.Add("dateTime", Configuration.ApiClient.ParameterToString(dateTime)); // form parameter
            if (password != null) localVarFormParams.Add("password", Configuration.ApiClient.ParameterToString(password)); // form parameter


            // make the HTTP request
            IRestResponse localVarResponse = (IRestResponse) await Configuration.ApiClient.CallApiAsync(localVarPath,
                Method.POST, localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarFileParams,
                localVarPathParams, localVarHttpContentType);

            int localVarStatusCode = (int) localVarResponse.StatusCode;

            if (localVarStatusCode >= 400)
                throw new ApiException (localVarStatusCode, "Error calling TestEndpointParameters: " + localVarResponse.Content, localVarResponse.Content);
            else if (localVarStatusCode == 0)
                throw new ApiException (localVarStatusCode, "Error calling TestEndpointParameters: " + localVarResponse.ErrorMessage, localVarResponse.ErrorMessage);

            
            return new ApiResponse<Object>(localVarStatusCode,
                localVarResponse.Headers.ToDictionary(x => x.Name, x => x.Value.ToString()),
                null);
        }

    }
}
