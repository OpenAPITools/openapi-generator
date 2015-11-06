package io.swagger.codegen.options;

import io.swagger.codegen.CodegenConstants;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class SwaggerYamlOptionsProvider implements OptionsProvider {
    public static final String SORT_PARAMS_VALUE = "false";
    public static final String ENSURE_UNIQUE_PARAMS_VALUE = "true";

    @Override
    public String getLanguage() {
        return "swagger-yaml";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, SORT_PARAMS_VALUE)
                .put(CodegenConstants.ENSURE_UNIQUE_PARAMS, ENSURE_UNIQUE_PARAMS_VALUE)
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
