/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using static System.Net.Mime.MediaTypeNames;

namespace Org.OpenAPITools.Test
{
    public class EchoServerResponseParser
    {
        public String method; // e.g. GET
        public String path; // e.g. /query/style_form/explode_true/object?id=12345
        public String protocol; // e.g. HTTP/1.1
        public Dictionary<String, String> headers = new Dictionary<String, String>();
        public String body; // e.g. <html><head></head><body>Hello World!</body></html>

        public EchoServerResponseParser(String response)
        {
            if (response == null)
            {
                throw new SystemException("Echo server response cannot be null");
            }

            String[] lines = Regex.Split(response, "\r\n|\r|\n");
            bool firstLine = true;
            bool bodyStart = false;
            StringBuilder bodyBuilder = new StringBuilder();

            foreach (String line in lines)
            {
                if (firstLine)
                {
                    String[] items = line.Split(" ");
                    this.method = items[0];
                    this.path = items[1];
                    this.protocol = items[2];
                    firstLine = false;
                    continue;
                }

                if (bodyStart)
                {
                    bodyBuilder.Append(line);
                    bodyBuilder.Append("\n");
                }

                if (String.IsNullOrEmpty(line))
                {
                    bodyStart = true;
                    continue;
                }

                // store the header key-value pair in headers
                String[] keyValue = line.Split(": ");
                if (keyValue.Length == 2)
                { // skip blank line, non key-value pair
                    this.headers.Add(keyValue[0], keyValue[1]);
                }
            }

            body = bodyBuilder.ToString();
        }
    }
}
