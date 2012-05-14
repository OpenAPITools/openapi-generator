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

namespace SwaggerRuntime.Exceptions
{
  /// <summary>
  /// An exception that is thrown if there are any issues while invoking the API.
  /// </summary>
  [Serializable]
  public class ApiException : Exception
  {
    public ApiException(int httpStatusCode)
      : base(string.Format("Api exception ({0}).", httpStatusCode))
    {
      HttpStatusCode = httpStatusCode;
    }

    public int HttpStatusCode { get; private set; }
  }
}
