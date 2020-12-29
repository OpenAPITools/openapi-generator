/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
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

package org.openapitools.codegen;

import java.util.*;

public class CodegenCallback {
    public String name;
    public List<Url> urls = new ArrayList<>();
    public Map<String, Object> vendorExtensions = new HashMap<>();

    public static class Url {
        public String expression;
        public List<CodegenOperation> requests = new ArrayList<>();
        public Map<String, Object> vendorExtensions = new HashMap<>();

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            Url that = (Url) o;
            return Objects.equals(that.expression, expression) &&
                    Objects.equals(that.requests, requests) && Objects.equals(that.vendorExtensions, vendorExtensions);
        }
        @Override
        public int hashCode() {
            return Objects.hash(expression, requests, vendorExtensions);
        }

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append("CodegenCallback.Urls {\n");
            sb.append("    expression: ").append(expression).append("\n");
            requests.forEach(r -> sb.append("   ").append(r).append("\n"));
            sb.append("}");
            return sb.toString();
        }
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        CodegenCallback that = (CodegenCallback) o;
        return Objects.equals(that.name, name) &&
                Objects.equals(that.urls, urls) && Objects.equals(that.vendorExtensions, vendorExtensions);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, urls, vendorExtensions);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("CodegenCallback {\n");
        sb.append("    name: ").append(name).append("\n");
        urls.forEach(u -> sb.append("   ").append(u).append("\n"));
        sb.append("}");
        return sb.toString();
    }


}
