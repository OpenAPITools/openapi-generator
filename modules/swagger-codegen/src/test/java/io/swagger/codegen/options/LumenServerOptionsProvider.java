package io.swagger.codegen.options;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.AbstractPhpCodegen;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class LumenServerOptionsProvider implements OptionsProvider {
    public static final String MODEL_PACKAGE_VALUE = "package";
    public static final String API_PACKAGE_VALUE = "apiPackage";
    public static final String SORT_PARAMS_VALUE = "false";
    public static final String ENSURE_UNIQUE_PARAMS_VALUE = "true";
    public static final String VARIABLE_NAMING_CONVENTION_VALUE = "snake_case";
    public static final String INVOKER_PACKAGE_VALUE = "lumen";
    public static final String PACKAGE_PATH_VALUE = "php";
    public static final String SRC_BASE_PATH_VALUE = "libPhp";
    public static final String GIT_USER_ID_VALUE = "gitSwaggerPhp";
    public static final String GIT_REPO_ID_VALUE = "git-swagger-php";
    public static final String ARTIFACT_VERSION_VALUE = "1.0.0-SNAPSHOT";
    public static final String ALLOW_UNICODE_IDENTIFIERS_VALUE = "false";


    @Override
    public String getLanguage() {
        return "lumen";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(CodegenConstants.MODEL_PACKAGE, MODEL_PACKAGE_VALUE)
                .put(AbstractPhpCodegen.VARIABLE_NAMING_CONVENTION, VARIABLE_NAMING_CONVENTION_VALUE)
                .put(AbstractPhpCodegen.PACKAGE_PATH, PACKAGE_PATH_VALUE)
                .put(AbstractPhpCodegen.SRC_BASE_PATH, SRC_BASE_PATH_VALUE)
                .put(CodegenConstants.API_PACKAGE, API_PACKAGE_VALUE)
                .put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, SORT_PARAMS_VALUE)
                .put(CodegenConstants.ENSURE_UNIQUE_PARAMS, ENSURE_UNIQUE_PARAMS_VALUE)
                .put(CodegenConstants.INVOKER_PACKAGE, INVOKER_PACKAGE_VALUE)
                .put(CodegenConstants.GIT_USER_ID, GIT_USER_ID_VALUE)
                .put(CodegenConstants.GIT_REPO_ID, GIT_REPO_ID_VALUE)
                .put(CodegenConstants.ARTIFACT_VERSION, ARTIFACT_VERSION_VALUE)
                .put(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS, ALLOW_UNICODE_IDENTIFIERS_VALUE)
                .build();
    }

    @Override
    public boolean isServer() {
        return true;
    }
}
