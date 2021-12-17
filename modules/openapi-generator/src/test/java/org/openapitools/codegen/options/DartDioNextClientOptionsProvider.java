/*
 * Copyright 2021 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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
import org.openapitools.codegen.languages.DartDioNextClientCodegen;

import java.util.Map;

public class DartDioNextClientOptionsProvider implements OptionsProvider {
    public static final String SORT_PARAMS_VALUE = "true";
    public static final String SORT_MODEL_PROPERTIES_VALUE = "false";
    public static final String ENSURE_UNIQUE_PARAMS_VALUE = "true";
    public static final String PUB_LIBRARY_VALUE = "openapi.api";
    public static final String PUB_NAME_VALUE = "openapi";
    public static final String PUB_VERSION_VALUE = "1.0.0-SNAPSHOT";
    public static final String PUB_DESCRIPTION_VALUE = "OpenAPI API client dart-dio";
    public static final String SOURCE_FOLDER_VALUE = "src";
    public static final String USE_ENUM_EXTENSION = "true";
    public static final String ALLOW_UNICODE_IDENTIFIERS_VALUE = "false";
    public static final String PREPEND_FORM_OR_BODY_PARAMETERS_VALUE = "true";
    public static final String PUB_AUTHOR_VALUE = "Author";
    public static final String PUB_AUTHOR_EMAIL_VALUE = "author@homepage";
    public static final String PUB_HOMEPAGE_VALUE = "Homepage";
    public static final String ENUM_UNKNOWN_DEFAULT_CASE_VALUE = "false";

    @Override
    public String getLanguage() {
        return "dart-dio-next";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, SORT_PARAMS_VALUE)
                .put(CodegenConstants.SORT_MODEL_PROPERTIES_BY_REQUIRED_FLAG, SORT_MODEL_PROPERTIES_VALUE)
                .put(CodegenConstants.ENSURE_UNIQUE_PARAMS, ENSURE_UNIQUE_PARAMS_VALUE)
                .put(DartDioNextClientCodegen.PUB_LIBRARY, PUB_LIBRARY_VALUE)
                .put(DartDioNextClientCodegen.PUB_NAME, PUB_NAME_VALUE)
                .put(DartDioNextClientCodegen.PUB_VERSION, PUB_VERSION_VALUE)
                .put(DartDioNextClientCodegen.PUB_DESCRIPTION, PUB_DESCRIPTION_VALUE)
                .put(DartDioNextClientCodegen.PUB_AUTHOR, PUB_AUTHOR_VALUE)
                .put(DartDioNextClientCodegen.PUB_AUTHOR_EMAIL, PUB_AUTHOR_EMAIL_VALUE)
                .put(DartDioNextClientCodegen.PUB_HOMEPAGE, PUB_HOMEPAGE_VALUE)
                .put(CodegenConstants.SERIALIZATION_LIBRARY, DartDioNextClientCodegen.SERIALIZATION_LIBRARY_DEFAULT)
                .put(DartDioNextClientCodegen.DATE_LIBRARY, DartDioNextClientCodegen.DATE_LIBRARY_DEFAULT)
                .put(CodegenConstants.SOURCE_FOLDER, SOURCE_FOLDER_VALUE)
                .put(DartDioNextClientCodegen.USE_ENUM_EXTENSION, USE_ENUM_EXTENSION)
                .put(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS, ALLOW_UNICODE_IDENTIFIERS_VALUE)
                .put(CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS, PREPEND_FORM_OR_BODY_PARAMETERS_VALUE)
                .put(CodegenConstants.LEGACY_DISCRIMINATOR_BEHAVIOR, "true")
                .put(CodegenConstants.DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT, "true")
                .put(CodegenConstants.ENUM_UNKNOWN_DEFAULT_CASE, ENUM_UNKNOWN_DEFAULT_CASE_VALUE)
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
