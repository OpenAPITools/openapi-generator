package org.openapitools.codegen.options;

import java.util.Map;

import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.languages.N4jsClientCodegen;

import com.google.common.collect.ImmutableMap;

public class N4jsClientCodegenOptionsProvider implements OptionsProvider {
    public static final String PROJECT_NAME_VALUE = "OpenAPI";
    public static final String CHECK_REQUIRED_PARAMS_NOT_NULL__VALUE = "true";
    public static final String CHECK_SUPERFLUOUS_BODY_PROPS__VALUE = "true";
    public static final String GENERATE_DEFAULT_API_EXECUTER__VALUE = "true";

    @Override
    public String getLanguage() {
        return "n4js";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder
                .put(N4jsClientCodegen.CHECK_REQUIRED_PARAMS_NOT_NULL, CHECK_REQUIRED_PARAMS_NOT_NULL__VALUE)
                .put(N4jsClientCodegen.CHECK_SUPERFLUOUS_BODY_PROPS, CHECK_SUPERFLUOUS_BODY_PROPS__VALUE)
                .put(N4jsClientCodegen.GENERATE_DEFAULT_API_EXECUTER, GENERATE_DEFAULT_API_EXECUTER__VALUE)
                .put(CodegenConstants.API_PACKAGE, "")
                .put(CodegenConstants.MODEL_PACKAGE, "")
                .put(CodegenConstants.API_NAME_PREFIX, "")
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}

