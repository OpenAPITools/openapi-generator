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
import org.openapitools.codegen.languages.Swift5ClientCodegen;

import java.util.Map;

public class Swift5OptionsProvider implements OptionsProvider {
    public static final String SORT_PARAMS_VALUE = "false";
    public static final String SORT_MODEL_PROPERTIES_VALUE = "false";
    public static final String ENSURE_UNIQUE_PARAMS_VALUE = "true";
    public static final String PROJECT_NAME_VALUE = "Swagger";
    public static final String RESPONSE_AS_VALUE = "test";
    public static final String NON_PUBLIC_API_REQUIRED_VALUE = "false";
    public static final String OBJC_COMPATIBLE_VALUE = "false";
    public static final String LENIENT_TYPE_CAST_VALUE = "false";
    public static final String POD_SOURCE_VALUE = "{ :git => 'git@github.com:swagger-api/swagger-mustache.git'," +
            " :tag => 'v1.0.0-SNAPSHOT' }";
    public static final String POD_VERSION_VALUE = "v1.0.0-SNAPSHOT";
    public static final String POD_AUTHORS_VALUE = "podAuthors";
    public static final String POD_SOCIAL_MEDIA_URL_VALUE = "podSocialMediaURL";
    public static final String POD_LICENSE_VALUE = "'Apache License, Version 2.0'";
    public static final String POD_HOMEPAGE_VALUE = "podHomepage";
    public static final String POD_SUMMARY_VALUE = "podSummary";
    public static final String POD_DESCRIPTION_VALUE = "podDescription";
    public static final String POD_SCREENSHOTS_VALUE = "podScreenshots";
    public static final String POD_DOCUMENTATION_URL_VALUE = "podDocumentationURL";
    public static final String READONLY_PROPERTIES_VALUE = "false";
    public static final String SWIFT_USE_API_NAMESPACE_VALUE = "swiftUseApiNamespace";
    public static final String ALLOW_UNICODE_IDENTIFIERS_VALUE = "false";
    public static final String PREPEND_FORM_OR_BODY_PARAMETERS_VALUE = "true";
    public static final String LIBRARY_VALUE = "alamofire";

    @Override
    public String getLanguage() {
        return "swift5";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, SORT_PARAMS_VALUE)
                .put(CodegenConstants.SORT_MODEL_PROPERTIES_BY_REQUIRED_FLAG, SORT_MODEL_PROPERTIES_VALUE)
                .put(CodegenConstants.ENSURE_UNIQUE_PARAMS, ENSURE_UNIQUE_PARAMS_VALUE)
                .put(Swift5ClientCodegen.PROJECT_NAME, PROJECT_NAME_VALUE)
                .put(Swift5ClientCodegen.RESPONSE_AS, RESPONSE_AS_VALUE)
                .put(CodegenConstants.NON_PUBLIC_API, NON_PUBLIC_API_REQUIRED_VALUE)
                .put(Swift5ClientCodegen.OBJC_COMPATIBLE, OBJC_COMPATIBLE_VALUE)
                .put(Swift5ClientCodegen.LENIENT_TYPE_CAST, LENIENT_TYPE_CAST_VALUE)
                .put(Swift5ClientCodegen.POD_SOURCE, POD_SOURCE_VALUE)
                .put(CodegenConstants.POD_VERSION, POD_VERSION_VALUE)
                .put(Swift5ClientCodegen.POD_AUTHORS, POD_AUTHORS_VALUE)
                .put(Swift5ClientCodegen.POD_SOCIAL_MEDIA_URL, POD_SOCIAL_MEDIA_URL_VALUE)
                .put(Swift5ClientCodegen.POD_LICENSE, POD_LICENSE_VALUE)
                .put(Swift5ClientCodegen.POD_HOMEPAGE, POD_HOMEPAGE_VALUE)
                .put(Swift5ClientCodegen.POD_SUMMARY, POD_SUMMARY_VALUE)
                .put(Swift5ClientCodegen.POD_DESCRIPTION, POD_DESCRIPTION_VALUE)
                .put(Swift5ClientCodegen.POD_SCREENSHOTS, POD_SCREENSHOTS_VALUE)
                .put(Swift5ClientCodegen.POD_DOCUMENTATION_URL, POD_DOCUMENTATION_URL_VALUE)
                .put(Swift5ClientCodegen.READONLY_PROPERTIES, READONLY_PROPERTIES_VALUE)
                .put(Swift5ClientCodegen.SWIFT_USE_API_NAMESPACE, SWIFT_USE_API_NAMESPACE_VALUE)
                .put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, "true")
                .put(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS, ALLOW_UNICODE_IDENTIFIERS_VALUE)
                .put(CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS, PREPEND_FORM_OR_BODY_PARAMETERS_VALUE)
                .put(CodegenConstants.API_NAME_PREFIX, "")
                .put(CodegenConstants.LIBRARY, LIBRARY_VALUE)
                .put(CodegenConstants.LEGACY_DISCRIMINATOR_BEHAVIOR, "true")
                .put(CodegenConstants.DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT, "true")
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
