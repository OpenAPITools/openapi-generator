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

package org.openapitools.client;

public class EchoServerResponseParser {
    public String method; // e.g. GET
    public String path; // e.g. /query/style_form/explode_true/object?id=12345
    public String protocol; // e.g. HTTP/1.1
    public java.util.HashMap<String, String> headers = new java.util.HashMap<>();

    public EchoServerResponseParser(String response) {
        if (response == null) {
            throw new RuntimeException("Echo server response cannot be null");
        }

        String[] lines = response.split("\n");
        boolean firstLine = true;

        for (String line : lines) {
            if (firstLine) {
                String[] items = line.split(" ");
                this.method = items[0];
                this.path = items[1];
                this.protocol = items[2];
                firstLine = false;
                continue;
            }

            // store the header key-value pair in headers
            String[] keyValue = line.split(": ");
            if (keyValue.length == 2) { // skip blank line, non key-value pair
                this.headers.put(keyValue[0], keyValue[1]);
            }
        }

    }
}
