package io.swagger.codegen.options;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.GoClientCodegen;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class GoClientOptionsProvider implements OptionsProvider {
    public static final String MODEL_PACKAGE_VALUE = "package";
    public static final String API_PACKAGE_VALUE = "apiPackage";
    public static final String SORT_PARAMS_VALUE = "false";
    public static final String ENSURE_UNIQUE_PARAMS_VALUE = "true";
    public static final String VARIABLE_NAMING_CONVENTION_VALUE = "snake_case";
    public static final String INVOKER_PACKAGE_VALUE = "Swagger\\Client\\Go";
    public static final String PACKAGE_PATH_VALUE = "SwaggerClient-go";
    public static final String SRC_BASE_PATH_VALUE = "libGo";
    public static final String COMPOSER_VENDOR_NAME_VALUE = "swaggerGo";
    public static final String COMPOSER_PROJECT_NAME_VALUE = "swagger-client-go";
    public static final String ARTIFACT_VERSION_VALUE = "1.0.0-SNAPSHOT";

    public static final String PACKAGE_VERSION_VALUE = "1.0.0-TEST";
    public static final String PACKAGE_NAME_VALUE = "TEST";

    @Override
    public String getLanguage() {
        return "go";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(CodegenConstants.MODEL_PACKAGE, MODEL_PACKAGE_VALUE)
                .put(CodegenConstants.API_PACKAGE, API_PACKAGE_VALUE)
                //.put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, SORT_PARAMS_VALUE)
                //.put(CodegenConstants.ENSURE_UNIQUE_PARAMS, ENSURE_UNIQUE_PARAMS_VALUE)
                .put(GoClientCodegen.VARIABLE_NAMING_CONVENTION, VARIABLE_NAMING_CONVENTION_VALUE)
                .put(CodegenConstants.INVOKER_PACKAGE, INVOKER_PACKAGE_VALUE)
                .put(GoClientCodegen.PACKAGE_PATH, PACKAGE_PATH_VALUE)
                .put(GoClientCodegen.SRC_BASE_PATH, SRC_BASE_PATH_VALUE)
                .put(GoClientCodegen.COMPOSER_VENDOR_NAME, COMPOSER_VENDOR_NAME_VALUE)
                .put(GoClientCodegen.COMPOSER_PROJECT_NAME, COMPOSER_PROJECT_NAME_VALUE)
                .put(CodegenConstants.ARTIFACT_VERSION, ARTIFACT_VERSION_VALUE)
                .put(CodegenConstants.PACKAGE_VERSION, PACKAGE_VERSION_VALUE)
                .put(CodegenConstants.PACKAGE_NAME, PACKAGE_NAME_VALUE)
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
