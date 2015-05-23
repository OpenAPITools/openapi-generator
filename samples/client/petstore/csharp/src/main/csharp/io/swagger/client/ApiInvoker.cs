using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Text;
using Newtonsoft.Json;

namespace IO.Swagger.Client {
  public class ApiInvoker {
    private static Dictionary<String, String> defaultHeaderMap = new Dictionary<String, String>();

    /// <summary>
    /// Add default header
    /// </summary>
    /// <param name="key">  Header field name
    /// <param name="value"> Header field value
    /// <returns></returns>
    public static void AddDefaultHeader(string key, string value) {
       defaultHeaderMap.Add(key, value);
    }

    /// <summary>
    /// Get default header
    /// </summary>
    /// <returns>Dictionary of default header</returns>
    public static Dictionary<String, String> GetDefaultHeader() {
       return defaultHeaderMap;
    }

    /// <summary>
    /// escape string (url-encoded)
    /// </summary>
    /// <param name="str"> String to be escaped
    /// <returns>Escaped string</returns>
    public static string EscapeString(string str) {
      return str;
    }

    /// <summary>
    /// if parameter is DateTime, output in ISO8601 format, otherwise just return the string
    /// </summary>
    /// <param name="obj"> The parameter (header, path, query, form)
    /// <returns>Formatted string</returns>
    public static string ParameterToString(object obj)
    {
      return (obj is DateTime) ? ((DateTime)obj).ToString ("u") : Convert.ToString (obj);
    }

    /// <summary>
    /// Deserialize the JSON string into a proper object
    /// </summary>
    /// <param name="json"> JSON string
    /// <param name="type"> Object type
    /// <returns>Object representation of the JSON string</returns>
    public static object Deserialize(string content, Type type) {
      if (type.GetType() == typeof(Object))
        return (Object)content;

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
    /// <param name="obj"> Object 
    /// <returns>JSON string</returns>
    public static string Serialize(object obj) {
      try
      {
          return obj != null ? JsonConvert.SerializeObject(obj) : null;
      }
      catch (Exception e) {
        throw new ApiException(500, e.Message);
      }
    }
  }
}
