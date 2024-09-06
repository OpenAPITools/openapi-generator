/*
package org.openapitools.codegen.options;

import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.languages.ScalaHttp4sScala3ClientCodegen;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class ScalaHttp4sScala3ClientCodegenOptionsProvider implements OptionsProvider {
    public static final String PROJECT_NAME_VALUE = "OpenAPI";
    public static final String MODEL_PACKAGE_VALUE = "package";
    public static final String SOURCE_FOLDER_VALUE = "sourceFolder";
    public static final String MODEL_PACKAGE_VALUE = "package";
    public static final String API_PACKAGE_VALUE = "apiPackage";
    public static final String SORT_PARAMS_VALUE = "false";
    public static final String SORT_MODEL_PROPERTIES_VALUE = "false";
    public static final String ENSURE_UNIQUE_PARAMS_VALUE = "true";
    public static final String MODEL_PROPERTY_NAMING = "PascalCase";
    public static final String ALLOW_UNICODE_IDENTIFIERS_VALUE = "false";
    public static final String PREPEND_FORM_OR_BODY_PARAMETERS_VALUE = "true";
    public static final String DATE_LIBRARY = "java8";

    @Override
    public String getLanguage() {
        return "scala-http4s-scala3";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(CodegenConstants.MODEL_PACKAGE, MODEL_PACKAGE_VALUE)
                .put(CodegenConstants.API_PACKAGE, API_PACKAGE_VALUE)
                .put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, SORT_PARAMS_VALUE)
                .put(CodegenConstants.SORT_MODEL_PROPERTIES_BY_REQUIRED_FLAG, SORT_MODEL_PROPERTIES_VALUE)
                .put(CodegenConstants.ENSURE_UNIQUE_PARAMS, ENSURE_UNIQUE_PARAMS_VALUE)
                .put(CodegenConstants.SOURCE_FOLDER, SOURCE_FOLDER_VALUE)
                .put(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS, ALLOW_UNICODE_IDENTIFIERS_VALUE)
                .put(CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS, PREPEND_FORM_OR_BODY_PARAMETERS_VALUE)
                .put(CodegenConstants.MODEL_PROPERTY_NAMING, MODEL_PROPERTY_NAMING)
                .put("dateLibrary", DATE_LIBRARY)
                .put(CodegenConstants.LEGACY_DISCRIMINATOR_BEHAVIOR, "true")
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}

*/
