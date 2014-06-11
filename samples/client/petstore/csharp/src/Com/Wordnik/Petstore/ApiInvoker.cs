  using System;
  using System.Collections.Generic;
  using System.IO;
  using System.Linq;
  using System.Net;
  using System.Text;
  using Newtonsoft.Json;

  namespace Com.Wordnik.Petstore {
    public class ApiInvoker {
      private static readonly ApiInvoker _instance = new ApiInvoker();
      private Dictionary<String, String> defaultHeaderMap = new Dictionary<String, String>();

      public static ApiInvoker GetInstance() {
        return _instance;
      }
      
      public void addDefaultHeader(string key, string value) {
         defaultHeaderMap.Add(key, value);
      }

      public string escapeString(string str) {
        return str;
      }

      public static object deserialize(string json, Type type) {
        try
        {
            return JsonConvert.DeserializeObject(json, type);
        }
        catch (IOException e) {
          throw new ApiException(500, e.Message);
        }

      }

      public static string serialize(object obj) {
        try
        {
            return obj != null ? JsonConvert.SerializeObject(obj) : null;
        }
        catch (Exception e) {
          throw new ApiException(500, e.Message);
        }
      }

      public string invokeAPI(string host, string path, string method, Dictionary<String, String> queryParams, object body, Dictionary<String, String> headerParams) {
        var b = new StringBuilder();
        
        foreach (var queryParamItem in queryParams)
        {
            var value = queryParamItem.Value;
            if (value == null) continue;
            b.Append(b.ToString().Length == 0 ? "?" : "&");
            b.Append(escapeString(queryParamItem.Key)).Append("=").Append(escapeString(value));
        }

        var querystring = b.ToString();

          host = host.EndsWith("/") ? host.Substring(0, host.Length - 1) : host;

          var client = WebRequest.Create(host + path + querystring);
          client.ContentType = "application/json";
          client.Method = method;

          foreach (var headerParamsItem in headerParams)
          {
              client.Headers.Add(headerParamsItem.Key, headerParamsItem.Value);
          }
          foreach (var defaultHeaderMapItem in defaultHeaderMap.Where(defaultHeaderMapItem => !headerParams.ContainsKey(defaultHeaderMapItem.Key)))
          {
              client.Headers.Add(defaultHeaderMapItem.Key, defaultHeaderMapItem.Value);
          }
    
          switch (method)
          {
              case "GET":
                  break;
              case "POST":
              case "PUT":
              case "DELETE":
                  var swRequestWriter = new StreamWriter(client.GetRequestStream());
                  swRequestWriter.Write(serialize(body));
                  swRequestWriter.Close();
                  break;
              default:
                  throw new ApiException(500, "unknown method type " + method);         
          }
              var webResponse = (HttpWebResponse) client.GetResponse();
              if (webResponse.StatusCode != HttpStatusCode.OK) throw new ApiException((int) webResponse.StatusCode, webResponse.StatusDescription);

              var responseReader = new StreamReader(webResponse.GetResponseStream());
              var responseData = responseReader.ReadToEnd();
              responseReader.Close();
              return responseData;
      }

    }
  }

