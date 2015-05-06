  using System;
  using System.Collections.Generic;
  using System.IO;
  using System.Linq;
  using System.Net;
  using System.Text;
  using Newtonsoft.Json;

  namespace io.swagger.client {
    public class ApiInvoker {
      private static readonly ApiInvoker _instance = new ApiInvoker();
      private Dictionary<String, String> defaultHeaderMap = new Dictionary<String, String>();

      public static ApiInvoker GetInstance() {
        return _instance;
      }

      /// <summary>
      /// Add default header
      /// </summary>
      /// <param name="key">  Header field name
      /// <param name="value"> Header field value
      /// <returns></returns>
      public void addDefaultHeader(string key, string value) {
         defaultHeaderMap.Add(key, value);
      }

      /// <summary>
      /// escape string (url-encoded)
      /// </summary>
      /// <param name="str"> String to be escaped
      /// <returns>Escaped string</returns>
      public string escapeString(string str) {
        return str;
      }

      /// <summary>
      /// if parameter is DateTime, output in ISO8601 format, otherwise just return the string
      /// </summary>
      /// <param name="obj"> The parameter (header, path, query, form)
      /// <returns>Formatted string</returns>
      public string ParameterToString(object obj)
      {
        return (obj is DateTime) ? ((DateTime)obj).ToString ("u") : Convert.ToString (obj);
      }

      /// <summary>
      /// Deserialize the JSON string into a proper object
      /// </summary>
      /// <param name="json"> JSON string
      /// <param name="type"> Object type
      /// <returns>Object representation of the JSON string</returns>
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

      public string invokeAPI(string host, string path, string method, Dictionary<String, String> queryParams, object body, Dictionary<String, String> headerParams, Dictionary<String, object> formParams)
      {
          return invokeAPIInternal(host, path, method, false, queryParams, body, headerParams, formParams) as string;
      }

      public byte[] invokeBinaryAPI(string host, string path, string method, Dictionary<String, String> queryParams, object body, Dictionary<String, String> headerParams, Dictionary<String, object> formParams)
      {
          return invokeAPIInternal(host, path, method, true, queryParams, body, headerParams, formParams) as byte[];
      }

      private object invokeAPIInternal(string host, string path, string method, bool binaryResponse, Dictionary<String, String> queryParams, object body, Dictionary<String, String> headerParams, Dictionary<String, object> formParams) {
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
          client.Method = method;

          byte[] formData = null;
          if (formParams.Count > 0)
          {
              string formDataBoundary = String.Format("----------{0:N}", Guid.NewGuid());
              client.ContentType = "multipart/form-data; boundary=" + formDataBoundary;
              formData = GetMultipartFormData(formParams, formDataBoundary);
              client.ContentLength = formData.Length;
          }
          else
          {
              client.ContentType = "application/json";
          }

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
              case "PATCH":
              case "PUT":
              case "DELETE":
                  using (Stream requestStream = client.GetRequestStream())
                  {
                      if (formData != null)
                      {
                          requestStream.Write(formData, 0, formData.Length);
                      }

                      var swRequestWriter = new StreamWriter(requestStream);
                      swRequestWriter.Write(serialize(body));
                      swRequestWriter.Close();
                  }
                  break;
              default:
                  throw new ApiException(500, "unknown method type " + method);
          }

          try
          {
              var webResponse = (HttpWebResponse)client.GetResponse();
              if (webResponse.StatusCode != HttpStatusCode.OK)
              {
                  webResponse.Close();
                  throw new ApiException((int)webResponse.StatusCode, webResponse.StatusDescription);
              }

              if (binaryResponse)
              {
                  using (var memoryStream = new MemoryStream())
                  {
                      webResponse.GetResponseStream().CopyTo(memoryStream);
                      return memoryStream.ToArray();
                  }
              }
              else
              {
                  using (var responseReader = new StreamReader(webResponse.GetResponseStream()))
                  {
                      var responseData = responseReader.ReadToEnd();
                      return responseData;
                  }
              }
          }
          catch(WebException ex)
          {
              var response = ex.Response as HttpWebResponse;
              int statusCode = 0;
              if (response != null)
              {
                  statusCode = (int)response.StatusCode;
                  response.Close();
              }
              throw new ApiException(statusCode, ex.Message);
          }
      }

      private static byte[] GetMultipartFormData(Dictionary<string, object> postParameters, string boundary)
      {
          Stream formDataStream = new System.IO.MemoryStream();
          bool needsCLRF = false;

          foreach (var param in postParameters)
          {
              // Thanks to feedback from commenters, add a CRLF to allow multiple parameters to be added.
              // Skip it on the first parameter, add it to subsequent parameters.
              if (needsCLRF)
                  formDataStream.Write(Encoding.UTF8.GetBytes("\r\n"), 0, Encoding.UTF8.GetByteCount("\r\n"));

              needsCLRF = true;

              if (param.Value is byte[])
              {
                  string postData = string.Format("--{0}\r\nContent-Disposition: form-data; name=\"{1}\"; filename=\"{1}\"\r\nContent-Type: {2}\r\n\r\n",
                      boundary,
                      param.Key,
                      "application/octet-stream");
                  formDataStream.Write(Encoding.UTF8.GetBytes(postData), 0, Encoding.UTF8.GetByteCount(postData));

                  // Write the file data directly to the Stream, rather than serializing it to a string.
                  formDataStream.Write((param.Value as byte[]), 0, (param.Value as byte[]).Length);
              }
              else
              {
                  string postData = string.Format("--{0}\r\nContent-Disposition: form-data; name=\"{1}\"\r\n\r\n{2}",
                      boundary,
                      param.Key,
                      param.Value);
                  formDataStream.Write(Encoding.UTF8.GetBytes(postData), 0, Encoding.UTF8.GetByteCount(postData));
              }
          }

          // Add the end of the request.  Start with a newline
          string footer = "\r\n--" + boundary + "--\r\n";
          formDataStream.Write(Encoding.UTF8.GetBytes(footer), 0, Encoding.UTF8.GetByteCount(footer));

          // Dump the Stream into a byte[]
          formDataStream.Position = 0;
          byte[] formData = new byte[formDataStream.Length];
          formDataStream.Read(formData, 0, formData.Length);
          formDataStream.Close();

          return formData;
      }
    }
  }
