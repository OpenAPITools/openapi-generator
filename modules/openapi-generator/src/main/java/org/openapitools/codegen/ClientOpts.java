/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen;

import org.openapitools.codegen.auth.AuthMethod;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public class ClientOpts {
    protected String uri;
    protected String target;
    protected AuthMethod auth;
    protected Map<String, String> properties = new HashMap<String, String>();
    protected String outputDirectory;

    public String getUri() {
        return uri;
    }

    public void setUri(String uri) {
        this.uri = uri;
    }

    public String getTarget() {
        return target;
    }

    public void setTarget(String target) {
        this.target = target;
    }

    public Map<String, String> getProperties() {
        return properties;
    }

    public void setProperties(Map<String, String> properties) {
        this.properties = properties;
    }

    public String getOutputDirectory() {
        return outputDirectory;
    }

    public void setOutputDirectory(String outputDirectory) {
        this.outputDirectory = outputDirectory;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("ClientOpts: {\n");
        sb.append("  uri: ").append(uri).append(",");
        sb.append("  auth: ").append(auth).append(",");
        sb.append(properties);
        sb.append("}");
        return sb.toString();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        ClientOpts that = (ClientOpts) o;
        return Objects.equals(uri, that.uri) &&
            Objects.equals(target, that.target) &&
            Objects.equals(auth, that.auth) &&
            Objects.equals(properties, that.properties) &&
            Objects.equals(outputDirectory, that.outputDirectory);
    }

    @Override
    public int hashCode() {
        return Objects.hash(uri, target, auth, properties, outputDirectory);
    }
}
