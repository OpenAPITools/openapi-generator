package io.swagger.codegen.options;

import com.google.common.collect.ImmutableMap;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.SymfonyServerCodegen;
import io.swagger.codegen.languages.SymfonyServerCodegen;

import java.util.Map;

public class SymfonyServerOptionsProvider implements OptionsProvider {
    public static final String BUNDLE_NAME_VALUE = "AcmeSwagger";
    public static final String MODEL_PACKAGE_VALUE = "package";
    public static final String API_PACKAGE_VALUE = "apiPackage";
    public static final String SORT_PARAMS_VALUE = "false";
    public static final String ENSURE_UNIQUE_PARAMS_VALUE = "true";
    public static final String VARIABLE_NAMING_CONVENTION_VALUE = "snake_case";
    public static final String INVOKER_PACKAGE_VALUE = "Acme\\Bundle\\SwaggerBundle";
    public static final String PACKAGE_PATH_VALUE = "SwaggerClient-php";
    public static final String SRC_BASE_PATH_VALUE = "libPhp";
    public static final String COMPOSER_VENDOR_NAME_VALUE = "swaggerPhp";
    public static final String COMPOSER_PROJECT_NAME_VALUE = "swagger-client-php";
    public static final String GIT_USER_ID_VALUE = "gitSwaggerPhp";
    public static final String GIT_REPO_ID_VALUE = "git-swagger-client-php";
    public static final String ARTIFACT_VERSION_VALUE = "1.0.0-SNAPSHOT";
    public static final String ALLOW_UNICODE_IDENTIFIERS_VALUE = "false";


    @Override
    public String getLanguage() {
        return "php-symfony";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(CodegenConstants.MODEL_PACKAGE, MODEL_PACKAGE_VALUE)
                .put(SymfonyServerCodegen.BUNDLE_NAME, BUNDLE_NAME_VALUE)
                .put(CodegenConstants.API_PACKAGE, API_PACKAGE_VALUE)
                .put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, SORT_PARAMS_VALUE)
                .put(CodegenConstants.ENSURE_UNIQUE_PARAMS, ENSURE_UNIQUE_PARAMS_VALUE)
                .put(SymfonyServerCodegen.VARIABLE_NAMING_CONVENTION, VARIABLE_NAMING_CONVENTION_VALUE)
                .put(CodegenConstants.INVOKER_PACKAGE, INVOKER_PACKAGE_VALUE)
                .put(SymfonyServerCodegen.PACKAGE_PATH, PACKAGE_PATH_VALUE)
                .put(SymfonyServerCodegen.SRC_BASE_PATH, SRC_BASE_PATH_VALUE)
                .put(SymfonyServerCodegen.COMPOSER_VENDOR_NAME, COMPOSER_VENDOR_NAME_VALUE)
                .put(CodegenConstants.GIT_USER_ID, GIT_USER_ID_VALUE)
                .put(SymfonyServerCodegen.COMPOSER_PROJECT_NAME, COMPOSER_PROJECT_NAME_VALUE)
                .put(CodegenConstants.GIT_REPO_ID, GIT_REPO_ID_VALUE)
                .put(CodegenConstants.ARTIFACT_VERSION, ARTIFACT_VERSION_VALUE)
                .put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, "true")
                .put(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS, ALLOW_UNICODE_IDENTIFIERS_VALUE)
                .build();
    }

    @Override
    public boolean isServer() {
        return true;
    }
}
