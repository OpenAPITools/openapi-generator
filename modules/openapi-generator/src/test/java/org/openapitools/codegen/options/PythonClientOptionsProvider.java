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

package org.openapitools.codegen.options;

import com.google.common.collect.ImmutableMap;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.languages.PythonClientCodegen;

import java.util.Map;

public class PythonClientOptionsProvider implements OptionsProvider {
    public static final String PACKAGE_NAME_VALUE = "swagger_client_python";
    public static final String PROJECT_NAME_VALUE = "swagger-client-python";
    public static final String PACKAGE_VERSION_VALUE = "1.0.0-SNAPSHOT";
    public static final String PACKAGE_URL_VALUE = "";
    public static final String USE_NOSE_VALUE = "false";
    public static final String RECURSION_LIMIT = "1200";
    public static final String DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT = "false";
    public static final String PYTHON_ATTR_NONE_IF_UNSET = "false";

    @Override
    public String getLanguage() {
        return "python";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(PythonClientCodegen.PACKAGE_URL, PACKAGE_URL_VALUE)
                .put(CodegenConstants.PACKAGE_NAME, PACKAGE_NAME_VALUE)
                .put(CodegenConstants.PROJECT_NAME, PROJECT_NAME_VALUE)
                .put(CodegenConstants.PACKAGE_VERSION, PACKAGE_VERSION_VALUE)
                .put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, "true")
                .put(CodegenConstants.SOURCECODEONLY_GENERATION, "false")
                .put(CodegenConstants.LIBRARY, "urllib3")
                .put(CodegenConstants.DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT, DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT)
                .put(PythonClientCodegen.USE_NOSE, USE_NOSE_VALUE)
                .put(PythonClientCodegen.RECURSION_LIMIT, RECURSION_LIMIT)
                .put(PythonClientCodegen.PYTHON_ATTR_NONE_IF_UNSET, PYTHON_ATTR_NONE_IF_UNSET)
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
