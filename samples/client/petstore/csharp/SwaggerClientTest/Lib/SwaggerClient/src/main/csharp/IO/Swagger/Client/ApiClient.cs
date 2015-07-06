using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.IO;
using System.Linq;
using System.Net;
using System.Text;
using System.Threading.Tasks;
using Newtonsoft.Json;
using RestSharp;
using RestSharp.Extensions;

namespace IO.Swagger.Client {
    /// <summary>
    /// API client is mainly responible for making the HTTP call to the API backend
    /// </summary>
    public class ApiClient {
  
        /// <summary>
        /// Initializes a new instance of the <see cref="ApiClient"/> class.
        /// </summary>
        /// <param name="basePath">The base path.</param>
        public ApiClient(String basePath="http://petstore.swagger.io/v2") {
            this.BasePath = basePath;
            this.RestClient = new RestClient(this.BasePath);
        }
    
        /// <summary>
        /// Gets or sets the base path.
        /// </summary>
        /// <value>The base path.</value> 
        public string BasePath { get; set; }
    
        /// <summary>
        /// Gets or sets the RestClient
        /// </summary>
        /// <value>The RestClient.</value> 
        public RestClient RestClient { get; set; }
    
        private Dictionary<String, String> DefaultHeaderMap = new Dictionary<String, String>();
    
        /// <summary>
        /// Make the HTTP request (Sync)
        /// </summary>
        /// <param name="path">URL path</param>
        /// <param name="method">HTTP method</param>
        /// <param name="queryParams">Query parameters</param>
        /// <param name="postBody">HTTP body (POST request)</param>
        /// <param name="headerParams">Header parameters</param>
        /// <param name="formParams">Form parameters</param>
        /// <param name="fileParams">File parameters</param>
        /// <param name="authSettings">Authentication settings</param>
        /// <returns>Object</returns>
        public Object CallApi(String path, RestSharp.Method method, Dictionary<String, String> queryParams, String postBody,
            Dictionary<String, String> headerParams, Dictionary<String, String> formParams, 
            Dictionary<String, FileParameter> fileParams, String[] authSettings) {

            var request = new RestRequest(path, method);
   
            UpdateParamsForAuth(queryParams, headerParams, authSettings);

            // add default header, if any
            foreach(KeyValuePair<string, string> defaultHeader in this.DefaultHeaderMap)
                request.AddHeader(defaultHeader.Key, defaultHeader.Value);

            // add header parameter, if any
            foreach(KeyValuePair<string, string> param in headerParams)
                request.AddHeader(param.Key, param.Value);

            // add query parameter, if any
            foreach(KeyValuePair<string, string> param in queryParams)
                request.AddQueryParameter(param.Key, param.Value);

            // add form parameter, if any
            foreach(KeyValuePair<string, string> param in formParams)
                request.AddParameter(param.Key, param.Value);

            // add file parameter, if any
            foreach(KeyValuePair<string, FileParameter> param in fileParams)
                request.AddFile(param.Value.Name, param.Value.Writer, param.Value.FileName, param.Value.ContentType);


            if (postBody != null) {
                request.AddParameter("application/json", postBody, ParameterType.RequestBody); // http body (model) parameter
            }

            return (Object)RestClient.Execute(request);

        }

        /// <summary>
        /// Make the HTTP request (Async)
        /// </summary>
        /// <param name="path">URL path</param>
        /// <param name="method">HTTP method</param>
        /// <param name="queryParams">Query parameters</param>
        /// <param name="postBody">HTTP body (POST request)</param>
        /// <param name="headerParams">Header parameters</param>
        /// <param name="formParams">Form parameters</param>
        /// <param name="fileParams">File parameters</param>
        /// <param name="authSettings">Authentication settings</param>
        /// <returns>Task</returns>
        public async Task<Object> CallApiAsync(String path, RestSharp.Method method, Dictionary<String, String> queryParams, String postBody,
            Dictionary<String, String> headerParams, Dictionary<String, String> formParams, Dictionary<String, FileParameter> fileParams, String[] authSettings) {
    
            var request = new RestRequest(path, method);
    
            UpdateParamsForAuth(queryParams, headerParams, authSettings);
    
            // add default header, if any
            foreach(KeyValuePair<string, string> defaultHeader in this.DefaultHeaderMap)
                request.AddHeader(defaultHeader.Key, defaultHeader.Value);
    
            // add header parameter, if any
            foreach(KeyValuePair<string, string> param in headerParams)
                request.AddHeader(param.Key, param.Value);
           
            // add query parameter, if any
            foreach(KeyValuePair<string, string> param in queryParams)
                request.AddQueryParameter(param.Key, param.Value);
    
            // add form parameter, if any
            foreach(KeyValuePair<string, string> param in formParams)
                request.AddParameter(param.Key, param.Value);
    
            // add file parameter, if any
            foreach(KeyValuePair<string, FileParameter> param in fileParams)
                request.AddFile(param.Value.Name, param.Value.Writer, param.Value.FileName, param.Value.ContentType);
    
    
            if (postBody != null) {
                request.AddParameter("application/json", postBody, ParameterType.RequestBody); // http body (model) parameter
            }
    
            return (Object) await RestClient.ExecuteTaskAsync(request);
    
        }
    
        /// <summary>
        /// Add default header
        /// </summary>
        /// <param name="key"> Header field name </param>
        /// <param name="value"> Header field value </param>
        /// <returns></returns>
        public void AddDefaultHeader(string key, string value) {
            DefaultHeaderMap.Add(key, value);
        }
    
        /// <summary>
        /// Get default header
        /// </summary>
        /// <returns>Dictionary of default header</returns>
        public Dictionary<String, String> GetDefaultHeader() {
            return DefaultHeaderMap;
        }
    
        /// <summary>
        /// escape string (url-encoded)
        /// </summary>
        /// <param name="str">String to be escaped</param>
        /// <returns>Escaped string</returns>
        public string EscapeString(string str) {
            return str;
        }
    
        /// <summary>
        /// Create FileParameter based on Stream
        /// </summary>
        /// <param name="name">parameter name</param>
        /// <param name="stream">Input stream</param>
        /// <returns>FileParameter</returns>
        public FileParameter ParameterToFile(string name, Stream stream)
        {
            if (stream is FileStream) {
                return FileParameter.Create(name, stream.ReadAsBytes(), Path.GetFileName(((FileStream)stream).Name));
            } else {
                return FileParameter.Create(name, stream.ReadAsBytes(), "no_file_name_provided");
            }
        }
    
        /// <summary>
        /// if parameter is DateTime, output in ISO8601 format
        /// if parameter is a list of string, join the list with ","
        /// otherwise just return the string
        /// </summary>
        /// <param name="obj">The parameter (header, path, query, form)</param>
        /// <returns>Formatted string</returns>
        public string ParameterToString(object obj)
        {
            if (obj is DateTime) {
                return ((DateTime)obj).ToString ("u");
            } else if (obj is List<string>) {
                return String.Join(",", obj as List<string>);
            } else {
                return Convert.ToString (obj);
            }
        }
    
        /// <summary>
        /// Deserialize the JSON string into a proper object
        /// </summary>
        /// <param name="content">HTTP body (e.g. string, JSON)</param>
        /// <param name="type">Object type</param>
        /// <returns>Object representation of the JSON string</returns>
        public object Deserialize(string content, Type type, IList<Parameter> headers=null) {
            if (type == typeof(Object)) { // return an object
                return (Object)content;
            } else if (type == typeof(Stream)) { 
                String fileName, filePath;
                if (String.IsNullOrEmpty (Configuration.TempFolderPath)) {
                    filePath = System.IO.Path.GetTempPath ();
                } else {
                    filePath = Configuration.TempFolderPath;
                }
    
                Regex regex = new Regex(@"Content-Disposition:.*filename=['""]?([^'""\s]+)['""]?$");
                Match match = regex.Match(headers.ToString());
                if (match.Success) {
                    // replace first and last " or ', if found
                    fileName = filePath + match.Value.Replace("\"", "").Replace("'","");
                } else {
                    fileName = filePath + Guid.NewGuid().ToString();
                }
                File.WriteAllText (fileName, content);
                return new FileStream(fileName, FileMode.Open);
            } else if (type.Name.StartsWith("System.Nullable`1[[System.DateTime")) { // return a datetime object
                return DateTime.Parse(content,  null, System.Globalization.DateTimeStyles.RoundtripKind);
            } else if (type.Name == "String" || type.Name.StartsWith("System.Nullable")) { // return primitive 
                return ConvertType(content, type); 
            }
    
            // at this point, it must be a model (json)
            try
            {
                return JsonConvert.DeserializeObject(content, type);
            }
            catch (IOException e) {
                throw new ApiException(500, e.Message);
            }
        }
    
        /// <summary>
        /// Serialize an object into JSON string
        /// </summary>
        /// <param name="obj">Object</param>
        /// <returns>JSON string</returns>
        public string Serialize(object obj) {
            try
            {
                return obj != null ? JsonConvert.SerializeObject(obj) : null;
            }
            catch (Exception e) {
                throw new ApiException(500, e.Message);
            }
        }
    
        /// <summary>
        /// Get the API key with prefix
        /// </summary>
        /// <param name="obj">Object</param>
        /// <returns>API key with prefix</returns>
        public string GetApiKeyWithPrefix (string apiKeyIdentifier)
        {
            var apiKeyValue = "";
            Configuration.ApiKey.TryGetValue (apiKeyIdentifier, out apiKeyValue);
            var apiKeyPrefix = "";
            if (Configuration.ApiKeyPrefix.TryGetValue (apiKeyIdentifier, out apiKeyPrefix)) {
                return apiKeyPrefix + " " + apiKeyValue;
            } else {
                return apiKeyValue;
            }
        }
    
        /// <summary>
        /// Update parameters based on authentication
        /// </summary>
        /// <param name="QueryParams">Query parameters</param>
        /// <param name="HeaderParams">Header parameters</param>
        /// <param name="AuthSettings">Authentication settings</param>
        public void UpdateParamsForAuth(Dictionary<String, String> queryParams, Dictionary<String, String> headerParams, string[] authSettings) {
            if (authSettings == null || authSettings.Length == 0)
                return;

            foreach (string auth in authSettings) {
                // determine which one to use
                switch(auth) {
                    
                    case "api_key":
                        headerParams["api_key"] = GetApiKeyWithPrefix("api_key");
                        
                        break;
                    
                    case "petstore_auth":
                        
                        //TODO support oauth
                        break;
                    
                    default:
                        //TODO show warning about security definition not found
                        break;
                }
            }

        }
 
        /// <summary>
        /// Encode string in base64 format 
        /// </summary>
        /// <param name="text">String to be encoded</param>
        public static string Base64Encode(string text) {
            var textByte = System.Text.Encoding.UTF8.GetBytes(text);
            return System.Convert.ToBase64String(textByte);
        }
    
        /// <summary>
        /// Dynamically cast the object into target type
        /// Ref: http://stackoverflow.com/questions/4925718/c-dynamic-runtime-cast
        /// </summary>
        /// <param name="dynamic">Object to be casted</param>
        /// <param name="dest">Target type</param>
        public static dynamic ConvertType(dynamic source, Type dest) {
            return Convert.ChangeType(source, dest);
        }
  
    }
}
