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

import java.util.Map;

public class GoClientOptionsProvider implements OptionsProvider {

    public static final String PACKAGE_VERSION_VALUE = "1.0.0";
    public static final String PACKAGE_NAME_VALUE = "Go";
    public static final boolean WITH_GO_CODEGEN_COMMENT_VALUE = true;
    public static final boolean WITH_XML_VALUE = true;
    public static final boolean ENUM_CLASS_PREFIX_VALUE = true;
    public static final Boolean PREPEND_FORM_OR_BODY_PARAMETERS_VALUE = true;
    public static final boolean IS_GO_SUBMODULE_VALUE = true;
    public static final boolean STRUCT_PREFIX_VALUE = true;
    public static final boolean WITH_AWSV4_SIGNATURE = true;

    @Override
    public String getLanguage() {
        return "go";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder
                .put(CodegenConstants.PACKAGE_VERSION, PACKAGE_VERSION_VALUE)
                .put(CodegenConstants.PACKAGE_NAME, PACKAGE_NAME_VALUE)
                .put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, "true")
                .put(CodegenConstants.WITH_GO_CODEGEN_COMMENT, "true")
                .put(CodegenConstants.WITH_XML, "true")
                .put(CodegenConstants.ENUM_CLASS_PREFIX, "true")
                .put(CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS, "true")
                .put(CodegenConstants.IS_GO_SUBMODULE, "true")
                .put(CodegenConstants.WITH_AWSV4_SIGNATURE_COMMENT, "true")
                .put("structPrefix", "true")
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
