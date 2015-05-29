using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Text;
using IO.Swagger.Client;

namespace IO.Swagger.Client {
  /// <summary>
  /// Represents a set of configuration settings
  /// </summary>
  public class Configuration{

    /// <summary>
    /// Gets or sets the API client. This is the default API client for making HTTP calls.
    /// </summary>
    /// <value>The API client.</value>
    public static ApiClient apiClient = new ApiClient();

    /// <summary>
    /// Gets or sets the username (HTTP basic authentication)
    /// </summary>
    /// <value>The username.</value>
    public static String username { get; set; }

    /// <summary>
    /// Gets or sets the password (HTTP basic authentication)
    /// </summary>
    /// <value>The password.</value>
    public static String password { get; set; }

    /// <summary>
    /// Gets or sets the API key based on the authentication name 
    /// </summary>
    /// <value>The API key.</value>
    public static Dictionary<String, String> apiKey = new Dictionary<String, String>();

    /// <summary>
    /// Gets or sets the prefix (e.g. Token) of the API key based on the authentication name 
    /// </summary>
    /// <value>The prefix of the API key.</value>
    public static Dictionary<String, String> apiKeyPrefix = new Dictionary<String, String>();


  }
}
