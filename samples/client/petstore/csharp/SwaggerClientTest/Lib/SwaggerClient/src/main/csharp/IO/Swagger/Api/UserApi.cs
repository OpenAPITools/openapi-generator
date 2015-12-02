using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;
using RestSharp;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace IO.Swagger.Api
{
    
    /// <summary>
    /// Represents a collection of functions to interact with the API endpoints
    /// </summary>
    public interface IUserApi
    {
        
        /// <summary>
        /// Create user
        /// </summary>
        /// <remarks>
        /// This can only be done by the logged in user.
        /// </remarks>
        /// <param name="body">Created user object</param>
        /// <returns></returns>
        void CreateUser (User body = null);
  
        /// <summary>
        /// Create user
        /// </summary>
        /// <remarks>
        /// This can only be done by the logged in user.
        /// </remarks>
        /// <param name="body">Created user object</param>
        /// <returns></returns>
        System.Threading.Tasks.Task CreateUserAsync (User body = null);
        
        /// <summary>
        /// Creates list of users with given input array
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="body">List of user object</param>
        /// <returns></returns>
        void CreateUsersWithArrayInput (List<User> body = null);
  
        /// <summary>
        /// Creates list of users with given input array
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="body">List of user object</param>
        /// <returns></returns>
        System.Threading.Tasks.Task CreateUsersWithArrayInputAsync (List<User> body = null);
        
        /// <summary>
        /// Creates list of users with given input array
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="body">List of user object</param>
        /// <returns></returns>
        void CreateUsersWithListInput (List<User> body = null);
  
        /// <summary>
        /// Creates list of users with given input array
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="body">List of user object</param>
        /// <returns></returns>
        System.Threading.Tasks.Task CreateUsersWithListInputAsync (List<User> body = null);
        
        /// <summary>
        /// Logs user into the system
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="username">The user name for login</param>
        /// <param name="password">The password for login in clear text</param>
        /// <returns>string</returns>
        string LoginUser (string username = null, string password = null);
  
        /// <summary>
        /// Logs user into the system
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="username">The user name for login</param>
        /// <param name="password">The password for login in clear text</param>
        /// <returns>string</returns>
        System.Threading.Tasks.Task<string> LoginUserAsync (string username = null, string password = null);
        
        /// <summary>
        /// Logs out current logged in user session
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <returns></returns>
        void LogoutUser ();
  
        /// <summary>
        /// Logs out current logged in user session
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <returns></returns>
        System.Threading.Tasks.Task LogoutUserAsync ();
        
        /// <summary>
        /// Get user by user name
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="username">The name that needs to be fetched. Use user1 for testing.</param>
        /// <returns>User</returns>
        User GetUserByName (string username);
  
        /// <summary>
        /// Get user by user name
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="username">The name that needs to be fetched. Use user1 for testing.</param>
        /// <returns>User</returns>
        System.Threading.Tasks.Task<User> GetUserByNameAsync (string username);
        
        /// <summary>
        /// Updated user
        /// </summary>
        /// <remarks>
        /// This can only be done by the logged in user.
        /// </remarks>
        /// <param name="username">name that need to be deleted</param>
        /// <param name="body">Updated user object</param>
        /// <returns></returns>
        void UpdateUser (string username, User body = null);
  
        /// <summary>
        /// Updated user
        /// </summary>
        /// <remarks>
        /// This can only be done by the logged in user.
        /// </remarks>
        /// <param name="username">name that need to be deleted</param>
        /// <param name="body">Updated user object</param>
        /// <returns></returns>
        System.Threading.Tasks.Task UpdateUserAsync (string username, User body = null);
        
        /// <summary>
        /// Delete user
        /// </summary>
        /// <remarks>
        /// This can only be done by the logged in user.
        /// </remarks>
        /// <param name="username">The name that needs to be deleted</param>
        /// <returns></returns>
        void DeleteUser (string username);
  
        /// <summary>
        /// Delete user
        /// </summary>
        /// <remarks>
        /// This can only be done by the logged in user.
        /// </remarks>
        /// <param name="username">The name that needs to be deleted</param>
        /// <returns></returns>
        System.Threading.Tasks.Task DeleteUserAsync (string username);
        
    }
  
    /// <summary>
    /// Represents a collection of functions to interact with the API endpoints
    /// </summary>
    public class UserApi : IUserApi
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="UserApi"/> class.
        /// </summary>
        /// <returns></returns>
        public UserApi(String basePath)
        {
            this.Configuration = new Configuration(new ApiClient(basePath));
        }
    
        /// <summary>
        /// Initializes a new instance of the <see cref="UserApi"/> class
        /// using Configuration object
        /// </summary>
        /// <param name="configuration">An instance of Configuration</param>
        /// <returns></returns>
        public UserApi(Configuration configuration = null)
        {
            if (configuration == null) // use the default one in Configuration
                this.Configuration = Configuration.Default; 
            else
                this.Configuration = configuration;
        }

        /// <summary>
        /// Gets the base path of the API client.
        /// </summary>
        /// <value>The base path</value>
        public String GetBasePath()
        {
            return this.Configuration.BasePath;
        }
    
        /// <summary>
        /// Gets or sets the configuration object
        /// </summary>
        /// <value>An instance of the Configuration</value>
        public Configuration Configuration {get; set;}

        /// <summary>
        /// Gets the status code of the previous request
        /// </summary>
        public int StatusCode { get; private set; }

        /// <summary>
        /// Gets the response headers of the previous request
        /// </summary>
        public Dictionary<String, String> ResponseHeaders { get; private set; } 

        private readonly Dictionary<String, String> _defaultHeaderMap = new Dictionary<String, String>();

        /// <summary>
        /// Gets the default header.
        /// </summary>
        public Dictionary<String, String> DefaultHeader
        {
            get { return _defaultHeaderMap; }
        }

        /// <summary>
        /// Add default header.
        /// </summary>
        /// <param name="key">Header field name.</param>
        /// <param name="value">Header field value.</param>
        /// <returns></returns>
        public void AddDefaultHeader(string key, string value)
        {
            _defaultHeaderMap.Add(key, value);
        }
   
        
        /// <summary>
        /// Create user This can only be done by the logged in user.
        /// </summary>
        /// <param name="body">Created user object</param> 
        /// <returns></returns>            
        public void CreateUser (User body = null)
        {
            
    
            var path_ = "/user";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>(Configuration.DefaultHeader);
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;

            // to determine the Accept header
            String[] http_header_accepts = new String[] {
                "application/json", "application/xml"
            };
            String http_header_accept = Configuration.ApiClient.SelectHeaderAccept(http_header_accepts);
            if (http_header_accept != null)
                headerParams.Add("Accept", Configuration.ApiClient.SelectHeaderAccept(http_header_accepts));

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            
            
            
            
            postBody = Configuration.ApiClient.Serialize(body); // http body (model) parameter
            

            
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) Configuration.ApiClient.CallApi(path_, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, pathParams);

            StatusCode = (int) response.StatusCode;
            ResponseHeaders = response.Headers.ToDictionary(x => x.Name, x => x.Value.ToString());
    
            if (StatusCode >= 400)
                throw new ApiException (StatusCode, "Error calling CreateUser: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException (StatusCode, "Error calling CreateUser: " + response.ErrorMessage, response.ErrorMessage);
    
            return;
        }
    
        /// <summary>
        /// Create user This can only be done by the logged in user.
        /// </summary>
        /// <param name="body">Created user object</param>
        /// <returns></returns>
        public async System.Threading.Tasks.Task CreateUserAsync (User body = null)
        {
            
    
            var path_ = "/user";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;

            // to determine the Accept header
            String[] http_header_accepts = new String[] {
                "application/json", "application/xml"
            };
            String http_header_accept = Configuration.ApiClient.SelectHeaderAccept(http_header_accepts);
            if (http_header_accept != null)
                headerParams.Add("Accept", Configuration.ApiClient.SelectHeaderAccept(http_header_accepts));

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            
            
            
            
            postBody = Configuration.ApiClient.Serialize(body); // http body (model) parameter
            

            

            // make the HTTP request
            IRestResponse response = (IRestResponse) await Configuration.ApiClient.CallApiAsync(path_, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, pathParams);

            StatusCode = (int) response.StatusCode;
            ResponseHeaders = response.Headers.ToDictionary(x => x.Name, x => x.Value.ToString());
 
            if (StatusCode >= 400)
                throw new ApiException (StatusCode, "Error calling CreateUser: " + response.Content, response.Content);

            
            return;
        }
        
        /// <summary>
        /// Creates list of users with given input array 
        /// </summary>
        /// <param name="body">List of user object</param> 
        /// <returns></returns>            
        public void CreateUsersWithArrayInput (List<User> body = null)
        {
            
    
            var path_ = "/user/createWithArray";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>(Configuration.DefaultHeader);
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;

            // to determine the Accept header
            String[] http_header_accepts = new String[] {
                "application/json", "application/xml"
            };
            String http_header_accept = Configuration.ApiClient.SelectHeaderAccept(http_header_accepts);
            if (http_header_accept != null)
                headerParams.Add("Accept", Configuration.ApiClient.SelectHeaderAccept(http_header_accepts));

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            
            
            
            
            postBody = Configuration.ApiClient.Serialize(body); // http body (model) parameter
            

            
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) Configuration.ApiClient.CallApi(path_, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, pathParams);

            StatusCode = (int) response.StatusCode;
            ResponseHeaders = response.Headers.ToDictionary(x => x.Name, x => x.Value.ToString());
    
            if (StatusCode >= 400)
                throw new ApiException (StatusCode, "Error calling CreateUsersWithArrayInput: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException (StatusCode, "Error calling CreateUsersWithArrayInput: " + response.ErrorMessage, response.ErrorMessage);
    
            return;
        }
    
        /// <summary>
        /// Creates list of users with given input array 
        /// </summary>
        /// <param name="body">List of user object</param>
        /// <returns></returns>
        public async System.Threading.Tasks.Task CreateUsersWithArrayInputAsync (List<User> body = null)
        {
            
    
            var path_ = "/user/createWithArray";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;

            // to determine the Accept header
            String[] http_header_accepts = new String[] {
                "application/json", "application/xml"
            };
            String http_header_accept = Configuration.ApiClient.SelectHeaderAccept(http_header_accepts);
            if (http_header_accept != null)
                headerParams.Add("Accept", Configuration.ApiClient.SelectHeaderAccept(http_header_accepts));

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            
            
            
            
            postBody = Configuration.ApiClient.Serialize(body); // http body (model) parameter
            

            

            // make the HTTP request
            IRestResponse response = (IRestResponse) await Configuration.ApiClient.CallApiAsync(path_, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, pathParams);

            StatusCode = (int) response.StatusCode;
            ResponseHeaders = response.Headers.ToDictionary(x => x.Name, x => x.Value.ToString());
 
            if (StatusCode >= 400)
                throw new ApiException (StatusCode, "Error calling CreateUsersWithArrayInput: " + response.Content, response.Content);

            
            return;
        }
        
        /// <summary>
        /// Creates list of users with given input array 
        /// </summary>
        /// <param name="body">List of user object</param> 
        /// <returns></returns>            
        public void CreateUsersWithListInput (List<User> body = null)
        {
            
    
            var path_ = "/user/createWithList";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>(Configuration.DefaultHeader);
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;

            // to determine the Accept header
            String[] http_header_accepts = new String[] {
                "application/json", "application/xml"
            };
            String http_header_accept = Configuration.ApiClient.SelectHeaderAccept(http_header_accepts);
            if (http_header_accept != null)
                headerParams.Add("Accept", Configuration.ApiClient.SelectHeaderAccept(http_header_accepts));

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            
            
            
            
            postBody = Configuration.ApiClient.Serialize(body); // http body (model) parameter
            

            
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) Configuration.ApiClient.CallApi(path_, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, pathParams);

            StatusCode = (int) response.StatusCode;
            ResponseHeaders = response.Headers.ToDictionary(x => x.Name, x => x.Value.ToString());
    
            if (StatusCode >= 400)
                throw new ApiException (StatusCode, "Error calling CreateUsersWithListInput: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException (StatusCode, "Error calling CreateUsersWithListInput: " + response.ErrorMessage, response.ErrorMessage);
    
            return;
        }
    
        /// <summary>
        /// Creates list of users with given input array 
        /// </summary>
        /// <param name="body">List of user object</param>
        /// <returns></returns>
        public async System.Threading.Tasks.Task CreateUsersWithListInputAsync (List<User> body = null)
        {
            
    
            var path_ = "/user/createWithList";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;

            // to determine the Accept header
            String[] http_header_accepts = new String[] {
                "application/json", "application/xml"
            };
            String http_header_accept = Configuration.ApiClient.SelectHeaderAccept(http_header_accepts);
            if (http_header_accept != null)
                headerParams.Add("Accept", Configuration.ApiClient.SelectHeaderAccept(http_header_accepts));

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            
            
            
            
            postBody = Configuration.ApiClient.Serialize(body); // http body (model) parameter
            

            

            // make the HTTP request
            IRestResponse response = (IRestResponse) await Configuration.ApiClient.CallApiAsync(path_, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, pathParams);

            StatusCode = (int) response.StatusCode;
            ResponseHeaders = response.Headers.ToDictionary(x => x.Name, x => x.Value.ToString());
 
            if (StatusCode >= 400)
                throw new ApiException (StatusCode, "Error calling CreateUsersWithListInput: " + response.Content, response.Content);

            
            return;
        }
        
        /// <summary>
        /// Logs user into the system 
        /// </summary>
        /// <param name="username">The user name for login</param> 
        /// <param name="password">The password for login in clear text</param> 
        /// <returns>string</returns>            
        public string LoginUser (string username = null, string password = null)
        {
            
    
            var path_ = "/user/login";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>(Configuration.DefaultHeader);
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;

            // to determine the Accept header
            String[] http_header_accepts = new String[] {
                "application/json", "application/xml"
            };
            String http_header_accept = Configuration.ApiClient.SelectHeaderAccept(http_header_accepts);
            if (http_header_accept != null)
                headerParams.Add("Accept", Configuration.ApiClient.SelectHeaderAccept(http_header_accepts));

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            
            if (username != null) queryParams.Add("username", Configuration.ApiClient.ParameterToString(username)); // query parameter
            if (password != null) queryParams.Add("password", Configuration.ApiClient.ParameterToString(password)); // query parameter
            
            
            
            

            
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) Configuration.ApiClient.CallApi(path_, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, pathParams);

            StatusCode = (int) response.StatusCode;
            ResponseHeaders = response.Headers.ToDictionary(x => x.Name, x => x.Value.ToString());
    
            if (StatusCode >= 400)
                throw new ApiException (StatusCode, "Error calling LoginUser: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException (StatusCode, "Error calling LoginUser: " + response.ErrorMessage, response.ErrorMessage);
    
            return (string) Configuration.ApiClient.Deserialize(response, typeof(string));
        }
    
        /// <summary>
        /// Logs user into the system 
        /// </summary>
        /// <param name="username">The user name for login</param>
        /// <param name="password">The password for login in clear text</param>
        /// <returns>string</returns>
        public async System.Threading.Tasks.Task<string> LoginUserAsync (string username = null, string password = null)
        {
            
    
            var path_ = "/user/login";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;

            // to determine the Accept header
            String[] http_header_accepts = new String[] {
                "application/json", "application/xml"
            };
            String http_header_accept = Configuration.ApiClient.SelectHeaderAccept(http_header_accepts);
            if (http_header_accept != null)
                headerParams.Add("Accept", Configuration.ApiClient.SelectHeaderAccept(http_header_accepts));

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            
            if (username != null) queryParams.Add("username", Configuration.ApiClient.ParameterToString(username)); // query parameter
            if (password != null) queryParams.Add("password", Configuration.ApiClient.ParameterToString(password)); // query parameter
            
            
            
            

            

            // make the HTTP request
            IRestResponse response = (IRestResponse) await Configuration.ApiClient.CallApiAsync(path_, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, pathParams);

            StatusCode = (int) response.StatusCode;
            ResponseHeaders = response.Headers.ToDictionary(x => x.Name, x => x.Value.ToString());
 
            if (StatusCode >= 400)
                throw new ApiException (StatusCode, "Error calling LoginUser: " + response.Content, response.Content);

            return (string) Configuration.ApiClient.Deserialize(response, typeof(string));
        }
        
        /// <summary>
        /// Logs out current logged in user session 
        /// </summary>
        /// <returns></returns>            
        public void LogoutUser ()
        {
            
    
            var path_ = "/user/logout";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>(Configuration.DefaultHeader);
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;

            // to determine the Accept header
            String[] http_header_accepts = new String[] {
                "application/json", "application/xml"
            };
            String http_header_accept = Configuration.ApiClient.SelectHeaderAccept(http_header_accepts);
            if (http_header_accept != null)
                headerParams.Add("Accept", Configuration.ApiClient.SelectHeaderAccept(http_header_accepts));

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            
            
            
            
            

            
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) Configuration.ApiClient.CallApi(path_, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, pathParams);

            StatusCode = (int) response.StatusCode;
            ResponseHeaders = response.Headers.ToDictionary(x => x.Name, x => x.Value.ToString());
    
            if (StatusCode >= 400)
                throw new ApiException (StatusCode, "Error calling LogoutUser: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException (StatusCode, "Error calling LogoutUser: " + response.ErrorMessage, response.ErrorMessage);
    
            return;
        }
    
        /// <summary>
        /// Logs out current logged in user session 
        /// </summary>
        /// <returns></returns>
        public async System.Threading.Tasks.Task LogoutUserAsync ()
        {
            
    
            var path_ = "/user/logout";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;

            // to determine the Accept header
            String[] http_header_accepts = new String[] {
                "application/json", "application/xml"
            };
            String http_header_accept = Configuration.ApiClient.SelectHeaderAccept(http_header_accepts);
            if (http_header_accept != null)
                headerParams.Add("Accept", Configuration.ApiClient.SelectHeaderAccept(http_header_accepts));

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            
            
            
            
            

            

            // make the HTTP request
            IRestResponse response = (IRestResponse) await Configuration.ApiClient.CallApiAsync(path_, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, pathParams);

            StatusCode = (int) response.StatusCode;
            ResponseHeaders = response.Headers.ToDictionary(x => x.Name, x => x.Value.ToString());
 
            if (StatusCode >= 400)
                throw new ApiException (StatusCode, "Error calling LogoutUser: " + response.Content, response.Content);

            
            return;
        }
        
        /// <summary>
        /// Get user by user name 
        /// </summary>
        /// <param name="username">The name that needs to be fetched. Use user1 for testing.</param> 
        /// <returns>User</returns>            
        public User GetUserByName (string username)
        {
            
            // verify the required parameter 'username' is set
            if (username == null) throw new ApiException(400, "Missing required parameter 'username' when calling GetUserByName");
            
    
            var path_ = "/user/{username}";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>(Configuration.DefaultHeader);
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;

            // to determine the Accept header
            String[] http_header_accepts = new String[] {
                "application/json", "application/xml"
            };
            String http_header_accept = Configuration.ApiClient.SelectHeaderAccept(http_header_accepts);
            if (http_header_accept != null)
                headerParams.Add("Accept", Configuration.ApiClient.SelectHeaderAccept(http_header_accepts));

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            if (username != null) pathParams.Add("username", Configuration.ApiClient.ParameterToString(username)); // path parameter
            
            
            
            
            

            
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) Configuration.ApiClient.CallApi(path_, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, pathParams);

            StatusCode = (int) response.StatusCode;
            ResponseHeaders = response.Headers.ToDictionary(x => x.Name, x => x.Value.ToString());
    
            if (StatusCode >= 400)
                throw new ApiException (StatusCode, "Error calling GetUserByName: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException (StatusCode, "Error calling GetUserByName: " + response.ErrorMessage, response.ErrorMessage);
    
            return (User) Configuration.ApiClient.Deserialize(response, typeof(User));
        }
    
        /// <summary>
        /// Get user by user name 
        /// </summary>
        /// <param name="username">The name that needs to be fetched. Use user1 for testing.</param>
        /// <returns>User</returns>
        public async System.Threading.Tasks.Task<User> GetUserByNameAsync (string username)
        {
            // verify the required parameter 'username' is set
            if (username == null) throw new ApiException(400, "Missing required parameter 'username' when calling GetUserByName");
            
    
            var path_ = "/user/{username}";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;

            // to determine the Accept header
            String[] http_header_accepts = new String[] {
                "application/json", "application/xml"
            };
            String http_header_accept = Configuration.ApiClient.SelectHeaderAccept(http_header_accepts);
            if (http_header_accept != null)
                headerParams.Add("Accept", Configuration.ApiClient.SelectHeaderAccept(http_header_accepts));

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            if (username != null) pathParams.Add("username", Configuration.ApiClient.ParameterToString(username)); // path parameter
            
            
            
            
            

            

            // make the HTTP request
            IRestResponse response = (IRestResponse) await Configuration.ApiClient.CallApiAsync(path_, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, pathParams);

            StatusCode = (int) response.StatusCode;
            ResponseHeaders = response.Headers.ToDictionary(x => x.Name, x => x.Value.ToString());
 
            if (StatusCode >= 400)
                throw new ApiException (StatusCode, "Error calling GetUserByName: " + response.Content, response.Content);

            return (User) Configuration.ApiClient.Deserialize(response, typeof(User));
        }
        
        /// <summary>
        /// Updated user This can only be done by the logged in user.
        /// </summary>
        /// <param name="username">name that need to be deleted</param> 
        /// <param name="body">Updated user object</param> 
        /// <returns></returns>            
        public void UpdateUser (string username, User body = null)
        {
            
            // verify the required parameter 'username' is set
            if (username == null) throw new ApiException(400, "Missing required parameter 'username' when calling UpdateUser");
            
    
            var path_ = "/user/{username}";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>(Configuration.DefaultHeader);
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;

            // to determine the Accept header
            String[] http_header_accepts = new String[] {
                "application/json", "application/xml"
            };
            String http_header_accept = Configuration.ApiClient.SelectHeaderAccept(http_header_accepts);
            if (http_header_accept != null)
                headerParams.Add("Accept", Configuration.ApiClient.SelectHeaderAccept(http_header_accepts));

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            if (username != null) pathParams.Add("username", Configuration.ApiClient.ParameterToString(username)); // path parameter
            
            
            
            
            postBody = Configuration.ApiClient.Serialize(body); // http body (model) parameter
            

            
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) Configuration.ApiClient.CallApi(path_, Method.PUT, queryParams, postBody, headerParams, formParams, fileParams, pathParams);

            StatusCode = (int) response.StatusCode;
            ResponseHeaders = response.Headers.ToDictionary(x => x.Name, x => x.Value.ToString());
    
            if (StatusCode >= 400)
                throw new ApiException (StatusCode, "Error calling UpdateUser: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException (StatusCode, "Error calling UpdateUser: " + response.ErrorMessage, response.ErrorMessage);
    
            return;
        }
    
        /// <summary>
        /// Updated user This can only be done by the logged in user.
        /// </summary>
        /// <param name="username">name that need to be deleted</param>
        /// <param name="body">Updated user object</param>
        /// <returns></returns>
        public async System.Threading.Tasks.Task UpdateUserAsync (string username, User body = null)
        {
            // verify the required parameter 'username' is set
            if (username == null) throw new ApiException(400, "Missing required parameter 'username' when calling UpdateUser");
            
    
            var path_ = "/user/{username}";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;

            // to determine the Accept header
            String[] http_header_accepts = new String[] {
                "application/json", "application/xml"
            };
            String http_header_accept = Configuration.ApiClient.SelectHeaderAccept(http_header_accepts);
            if (http_header_accept != null)
                headerParams.Add("Accept", Configuration.ApiClient.SelectHeaderAccept(http_header_accepts));

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            if (username != null) pathParams.Add("username", Configuration.ApiClient.ParameterToString(username)); // path parameter
            
            
            
            
            postBody = Configuration.ApiClient.Serialize(body); // http body (model) parameter
            

            

            // make the HTTP request
            IRestResponse response = (IRestResponse) await Configuration.ApiClient.CallApiAsync(path_, Method.PUT, queryParams, postBody, headerParams, formParams, fileParams, pathParams);

            StatusCode = (int) response.StatusCode;
            ResponseHeaders = response.Headers.ToDictionary(x => x.Name, x => x.Value.ToString());
 
            if (StatusCode >= 400)
                throw new ApiException (StatusCode, "Error calling UpdateUser: " + response.Content, response.Content);

            
            return;
        }
        
        /// <summary>
        /// Delete user This can only be done by the logged in user.
        /// </summary>
        /// <param name="username">The name that needs to be deleted</param> 
        /// <returns></returns>            
        public void DeleteUser (string username)
        {
            
            // verify the required parameter 'username' is set
            if (username == null) throw new ApiException(400, "Missing required parameter 'username' when calling DeleteUser");
            
    
            var path_ = "/user/{username}";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>(Configuration.DefaultHeader);
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;

            // to determine the Accept header
            String[] http_header_accepts = new String[] {
                "application/json", "application/xml"
            };
            String http_header_accept = Configuration.ApiClient.SelectHeaderAccept(http_header_accepts);
            if (http_header_accept != null)
                headerParams.Add("Accept", Configuration.ApiClient.SelectHeaderAccept(http_header_accepts));

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            if (username != null) pathParams.Add("username", Configuration.ApiClient.ParameterToString(username)); // path parameter
            
            
            
            
            

            
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) Configuration.ApiClient.CallApi(path_, Method.DELETE, queryParams, postBody, headerParams, formParams, fileParams, pathParams);

            StatusCode = (int) response.StatusCode;
            ResponseHeaders = response.Headers.ToDictionary(x => x.Name, x => x.Value.ToString());
    
            if (StatusCode >= 400)
                throw new ApiException (StatusCode, "Error calling DeleteUser: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException (StatusCode, "Error calling DeleteUser: " + response.ErrorMessage, response.ErrorMessage);
    
            return;
        }
    
        /// <summary>
        /// Delete user This can only be done by the logged in user.
        /// </summary>
        /// <param name="username">The name that needs to be deleted</param>
        /// <returns></returns>
        public async System.Threading.Tasks.Task DeleteUserAsync (string username)
        {
            // verify the required parameter 'username' is set
            if (username == null) throw new ApiException(400, "Missing required parameter 'username' when calling DeleteUser");
            
    
            var path_ = "/user/{username}";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;

            // to determine the Accept header
            String[] http_header_accepts = new String[] {
                "application/json", "application/xml"
            };
            String http_header_accept = Configuration.ApiClient.SelectHeaderAccept(http_header_accepts);
            if (http_header_accept != null)
                headerParams.Add("Accept", Configuration.ApiClient.SelectHeaderAccept(http_header_accepts));

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            if (username != null) pathParams.Add("username", Configuration.ApiClient.ParameterToString(username)); // path parameter
            
            
            
            
            

            

            // make the HTTP request
            IRestResponse response = (IRestResponse) await Configuration.ApiClient.CallApiAsync(path_, Method.DELETE, queryParams, postBody, headerParams, formParams, fileParams, pathParams);

            StatusCode = (int) response.StatusCode;
            ResponseHeaders = response.Headers.ToDictionary(x => x.Name, x => x.Value.ToString());
 
            if (StatusCode >= 400)
                throw new ApiException (StatusCode, "Error calling DeleteUser: " + response.Content, response.Content);

            
            return;
        }
        
    }
    
}
