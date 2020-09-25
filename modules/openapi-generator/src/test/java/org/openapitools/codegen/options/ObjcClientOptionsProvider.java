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
import org.openapitools.codegen.languages.ObjcClientCodegen;

import java.util.Map;

public class ObjcClientOptionsProvider implements OptionsProvider {
    public static final String CLASS_PREFIX_VALUE = "SWGObjc";
    public static final String CORE_DATA_VALUE = "n";
    public static final String POD_NAME_VALUE = "SwaggerClientObjc";
    public static final String POD_VERSION_VALUE = "1.0.0-SNAPSHOT";
    public static final String AUTHOR_NAME_VALUE = "SwaggerObjc";
    public static final String AUTHOR_EMAIL_VALUE = "objc@openapitools.org";
    public static final String GIT_REPO_URL_VALUE = "https://github.com/openapitools/openapi-generator";

    @Override
    public String getLanguage() {
        return "objc";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(ObjcClientCodegen.CLASS_PREFIX, CLASS_PREFIX_VALUE)
                .put(ObjcClientCodegen.POD_NAME, POD_NAME_VALUE)
                .put(CodegenConstants.POD_VERSION, POD_VERSION_VALUE)
                .put(ObjcClientCodegen.AUTHOR_NAME, AUTHOR_NAME_VALUE)
                .put(ObjcClientCodegen.AUTHOR_EMAIL, AUTHOR_EMAIL_VALUE)
                .put(ObjcClientCodegen.GIT_REPO_URL, GIT_REPO_URL_VALUE)
                .put(ObjcClientCodegen.CORE_DATA, CORE_DATA_VALUE)
                .put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, "true")
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
