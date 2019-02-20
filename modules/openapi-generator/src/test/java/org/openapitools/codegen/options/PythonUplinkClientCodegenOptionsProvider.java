/*
 * Copyright 2019 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

package org.openapitools.codegen.options;

import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.languages.PythonUplinkClientCodegen;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class PythonUplinkClientCodegenOptionsProvider implements OptionsProvider {
    public static final String PACKAGE_NAME_VALUE = "openapi_client_python_uplink";
    public static final String PROJECT_NAME_VALUE = "openapi-client-python-uplink";
    public static final String PACKAGE_VERSION_VALUE = "0.1.0";
    public static final String PACKAGE_URL_VALUE = "";
    public static final String CLIENT_CLASS_NAME = "ApiClient";

    @Override
    public String getLanguage() {
        return "python-uplink";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder
                .put(CodegenConstants.PACKAGE_NAME, PACKAGE_NAME_VALUE)
                .put(CodegenConstants.PROJECT_NAME, PROJECT_NAME_VALUE)
                .put(CodegenConstants.PACKAGE_VERSION, PACKAGE_VERSION_VALUE)
                .put(PythonUplinkClientCodegen.PACKAGE_URL, PACKAGE_URL_VALUE)
                .put(PythonUplinkClientCodegen.CLIENT_CLASS_NAME, CLIENT_CLASS_NAME)
                .put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, "true")
                .put(CodegenConstants.SOURCECODEONLY_GENERATION, "false")
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}

