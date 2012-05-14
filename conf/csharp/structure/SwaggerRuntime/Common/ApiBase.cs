/**
 *  Copyright 2011 Wordnik, Inc.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

using System;
using System.Collections.Generic;
using System.IO;
using System.Net;
using System.Text;
using System.Web;
using Newtonsoft.Json;
using SwaggerRuntime.Exceptions;

namespace SwaggerRuntime.Common
{
  public abstract class ApiBase
  {
    private readonly string _apiBaseUrl;
    private readonly ISecurityHandler _securityHandler;

    protected ApiBase(string apiBaseUrl, ISecurityHandler securityHandler)
    {
      if (string.IsNullOrEmpty(apiBaseUrl))
      {
        throw new ArgumentException("Argument can't be null nor empty.", "apiBaseUrl");
      }

      if (securityHandler == null)
      {
        throw new ArgumentNullException("securityHandler");
      }

      _apiBaseUrl = apiBaseUrl;
      _securityHandler = securityHandler;
    }

    protected string InvokeApi(string resourceUrl, string method, IDictionary<string, string> queryParams, Object postData, IDictionary<string, string> headerParams)
    {
      string absoluteResourceUrl = _apiBaseUrl + resourceUrl;

      if (queryParams.Count > 0)
      {
        bool isFirst = true;

        foreach (string queryParamName in queryParams.Keys)
        {
          absoluteResourceUrl += isFirst ? "?" : "&";
          isFirst = false;

          absoluteResourceUrl += queryParamName + "=" + EncodeUrl(queryParams[queryParamName]);
        }
      }

      var headers = new Dictionary<string, string>();

      _securityHandler.PopulateSecurityInfo(absoluteResourceUrl, headers);

      var request = (HttpWebRequest)WebRequest.Create(absoluteResourceUrl);
      
      request.Method = method;

      foreach (KeyValuePair<string, string> headerKvp in headers)
      {
        request.Headers[headerKvp.Key] = headerKvp.Value;
      }

      if (headerParams != null)
      {
        foreach (KeyValuePair<string, string> headerKvp in headerParams)
        {
          request.Headers[headerKvp.Key] = headerKvp.Value;
        }
      }

      using (var response = (HttpWebResponse)request.GetResponse())
      using (Stream responseStream = response.GetResponseStream())
      {
        if (responseStream == null)
        {
          throw new IOException("Couldn't get response stream.");
        }

        if (response.StatusCode != HttpStatusCode.OK)
        {
          throw new ApiException((int)response.StatusCode);
        }

        using (var sr = new StreamReader(responseStream))
        {
          return sr.ReadToEnd();
        }
      }
    }

    protected T Deserialize<T>(string response)
    {
      var jsonSerializer = new JsonSerializer();

      using (var sr = new StringReader(response))
      using (var jtr = new JsonTextReader(sr))
      {
        return jsonSerializer.Deserialize<T>(jtr);
      }
    }

    protected string Serialize(object input)
    {
      var jsonSerializer = new JsonSerializer();
      var sb = new StringBuilder();

      using (var sw = new StringWriter(sb))
      {
        jsonSerializer.Serialize(sw, input);
      }

      return sb.ToString();
    }

    protected string ToPathValue(string value)
    {
      return EncodeUrl(value ?? "");
    }

    protected string ToPathValue(IEnumerable<object> objects)
    {
      StringBuilder outSb = new StringBuilder();

      foreach (object obj in objects)
      {
        outSb.Append(obj.ToString());
        outSb.Append(",");
      }

      string output = outSb.ToString();

      if (output.IndexOf(",") != -1)
      {
        output = output.Substring(0, output.LastIndexOf(","));
      }

      return EncodeUrl(output);
    }

    protected string EncodeUrl(string value)
    {
      return HttpUtility.UrlEncode(value);
    }
  }
}
