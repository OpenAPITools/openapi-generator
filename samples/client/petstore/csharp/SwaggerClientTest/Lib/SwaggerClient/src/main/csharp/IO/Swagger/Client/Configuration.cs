using System;
using System.Reflection;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

namespace IO.Swagger.Client
{
    /// <summary>
    /// Represents a set of configuration settings
    /// </summary>
    public class Configuration
    {
  
        /// <summary>
        /// Version of the package.
        /// </summary>
        /// <value>Version of the package.</value>
        public const string Version = "1.0.0";
  
        /// <summary>
        /// Gets or sets the default API client for making HTTP calls.
        /// </summary>
        /// <value>The API client.</value>
        public static ApiClient DefaultApiClient = new ApiClient();
  
        /// <summary>
        /// Gets or sets the username (HTTP basic authentication).
        /// </summary>
        /// <value>The username.</value>
        public static String Username { get; set; }
  
        /// <summary>
        /// Gets or sets the password (HTTP basic authentication).
        /// </summary>
        /// <value>The password.</value>
        public static String Password { get; set; }
  
        /// <summary>
        /// Gets or sets the access token for OAuth2 authentication.
        /// </summary>
        /// <value>The access token.</value>
        public static String AccessToken { get; set; }

        /// <summary>
        /// Gets or sets the API key based on the authentication name.
        /// </summary>
        /// <value>The API key.</value>
        public static Dictionary<String, String> ApiKey = new Dictionary<String, String>();
  
        /// <summary>
        /// Gets or sets the prefix (e.g. Token) of the API key based on the authentication name.
        /// </summary>
        /// <value>The prefix of the API key.</value>
        public static Dictionary<String, String> ApiKeyPrefix = new Dictionary<String, String>();
  
        private static string _tempFolderPath = Path.GetTempPath();
  
        /// <summary>
        /// Gets or sets the temporary folder path to store the files downloaded from the server.
        /// </summary>
        /// <value>Folder path.</value>
        public static String TempFolderPath
        {
            get { return _tempFolderPath; }
  
            set 
            {
                if (String.IsNullOrEmpty(value))
                {
                    _tempFolderPath = value;
                    return;
                }
      
                // create the directory if it does not exist
                if (!Directory.Exists(value)) 
                    Directory.CreateDirectory(value);
      
                // check if the path contains directory separator at the end
                if (value[value.Length - 1] == Path.DirectorySeparatorChar)
                    _tempFolderPath = value;
                else
                    _tempFolderPath = value  + Path.DirectorySeparatorChar;
            }
        }
  
        /// <summary>
        /// Returns a string with essential information for debugging.
        /// </summary>
        public static String ToDebugReport()
        {
            String report = "C# SDK (IO.Swagger) Debug Report:\n";
            report += "    OS: " + Environment.OSVersion + "\n";
            report += "    .NET Framework Version: " + Assembly
                     .GetExecutingAssembly()
                     .GetReferencedAssemblies()
                     .Where(x => x.Name == "System.Core").First().Version.ToString()  + "\n";
            report += "    Version of the API: 1.0.0\n";
            report += "    SDK Package Version: 1.0.0\n";
  
            return report;
        }
    }
}
