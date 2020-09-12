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
import org.openapitools.codegen.languages.RubyClientCodegen;

import java.util.Map;

public class RubyClientOptionsProvider implements OptionsProvider {
    public static final String GEM_NAME_VALUE = "swagger_client_ruby";
    public static final String MODULE_NAME_VALUE = "SwaggerClientRuby";
    public static final String GEM_VERSION_VALUE = "1.0.0-SNAPSHOT";
    public static final String SORT_PARAMS_VALUE = "false";
    public static final String SORT_MODEL_PROPERTIES_VALUE = "false";
    public static final String ENSURE_UNIQUE_PARAMS_VALUE = "true";
    public static final String GEM_LICENSE_VALUE = "MIT";
    public static final String GEM_REQUIRED_RUBY_VERSION_VALUE = ">= 1.9";
    public static final String GEM_HOMEPAGE_VALUE = "homepage";
    public static final String GEM_SUMMARY_VALUE = "summary";
    public static final String GEM_DESCRIPTION_VALUE = "description";
    public static final String GEM_AUTHOR_VALUE =  "foo";
    public static final String GEM_AUTHOR_EMAIL_VALUE = "foo";
    public static final String ALLOW_UNICODE_IDENTIFIERS_VALUE = "false";
    public static final String PREPEND_FORM_OR_BODY_PARAMETERS_VALUE = "true";
    public static final String LIBRARY = "typhoeus";

    @Override
    public String getLanguage() {
        return "ruby";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(CodegenConstants.GEM_NAME, GEM_NAME_VALUE)
                .put(CodegenConstants.MODULE_NAME, MODULE_NAME_VALUE)
                .put(RubyClientCodegen.GEM_VERSION, GEM_VERSION_VALUE)
                .put(RubyClientCodegen.GEM_LICENSE, GEM_LICENSE_VALUE)
                .put(RubyClientCodegen.GEM_REQUIRED_RUBY_VERSION, GEM_REQUIRED_RUBY_VERSION_VALUE)
                .put(RubyClientCodegen.GEM_DESCRIPTION, GEM_DESCRIPTION_VALUE)
                .put(RubyClientCodegen.GEM_HOMEPAGE, GEM_HOMEPAGE_VALUE)
                .put(RubyClientCodegen.GEM_SUMMARY, GEM_SUMMARY_VALUE)
                .put(RubyClientCodegen.GEM_AUTHOR, GEM_AUTHOR_VALUE)
                .put(RubyClientCodegen.GEM_AUTHOR_EMAIL, GEM_AUTHOR_EMAIL_VALUE)
                .put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, SORT_PARAMS_VALUE)
                .put(CodegenConstants.SORT_MODEL_PROPERTIES_BY_REQUIRED_FLAG, SORT_MODEL_PROPERTIES_VALUE)
                .put(CodegenConstants.ENSURE_UNIQUE_PARAMS, ENSURE_UNIQUE_PARAMS_VALUE)
                .put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, "true")
                .put(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS, ALLOW_UNICODE_IDENTIFIERS_VALUE)
                .put(CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS, PREPEND_FORM_OR_BODY_PARAMETERS_VALUE)
                .put(CodegenConstants.LIBRARY, LIBRARY)
                .put(CodegenConstants.LEGACY_DISCRIMINATOR_BEHAVIOR, "true")
                .put(CodegenConstants.DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT, "true")
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
