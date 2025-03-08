package org.openapitools.codegen.options;

import com.google.common.collect.ImmutableMap;
import org.openapitools.codegen.CodegenConstants;

import java.io.File;
import java.util.Map;

public class ScalaHttp4sClientCodegenOptionsProvider implements OptionsProvider {
    public static final String ARTIFACT_ID_VALUE = "scala-http4s-client";
    public static final String PACKAGE_NAME_VALUE = "org.openapitools.client";
    public static final String MODEL_PACKAGE_VALUE = PACKAGE_NAME_VALUE + ".models";
    public static final String API_PACKAGE_VALUE = PACKAGE_NAME_VALUE + ".apis";
    public static final String MODEL_PROPERTY_NAMING = "camelCase";
    public static final String SOURCE_FOLDER_VALUE = "src" + File.separator + "main" + File.separator + "scala";
    public static final String SORT_PARAMS_VALUE = "false";
    public static final String SORT_MODEL_PROPERTIES_VALUE = "false";
    public static final String ENSURE_UNIQUE_PARAMS_VALUE = "true";
    public static final String ALLOW_UNICODE_IDENTIFIERS_VALUE = "false";
    public static final String PREPEND_FORM_OR_BODY_PARAMETERS_VALUE = "true";
    public static final String DATE_LIBRARY = "java8";

    @Override
    public String getLanguage() {
        return "scala-http4s";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder
                .put(CodegenConstants.ARTIFACT_ID, ARTIFACT_ID_VALUE)
                .put(CodegenConstants.PACKAGE_NAME, PACKAGE_NAME_VALUE)
                .put(CodegenConstants.MODEL_PACKAGE, MODEL_PACKAGE_VALUE)
                .put(CodegenConstants.API_PACKAGE, API_PACKAGE_VALUE)
                .put(CodegenConstants.MODEL_PROPERTY_NAMING, MODEL_PROPERTY_NAMING)
                .put(CodegenConstants.SOURCE_FOLDER, SOURCE_FOLDER_VALUE)
                .put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, SORT_PARAMS_VALUE)
                .put(CodegenConstants.SORT_MODEL_PROPERTIES_BY_REQUIRED_FLAG, SORT_MODEL_PROPERTIES_VALUE)
                .put(CodegenConstants.ENSURE_UNIQUE_PARAMS, ENSURE_UNIQUE_PARAMS_VALUE)
                .put(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS, ALLOW_UNICODE_IDENTIFIERS_VALUE)
                .put(CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS, PREPEND_FORM_OR_BODY_PARAMETERS_VALUE)
                .put("dateLibrary", DATE_LIBRARY)
                .put(CodegenConstants.LEGACY_DISCRIMINATOR_BEHAVIOR, "true")
                .put("excludeSbt", "false")
                .put("excludeApi", "false")
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}