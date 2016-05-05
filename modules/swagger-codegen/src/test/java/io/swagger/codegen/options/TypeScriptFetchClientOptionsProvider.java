package io.swagger.codegen.options;

import com.google.common.collect.ImmutableMap;
import io.swagger.codegen.CodegenConstants;

import java.util.Map;

public class TypeScriptFetchClientOptionsProvider implements OptionsProvider {
    public static final String SORT_PARAMS_VALUE = "false";
    public static final String ENSURE_UNIQUE_PARAMS_VALUE = "true";
    public static final Boolean SUPPORTS_ES6_VALUE = false;
    public static final String MODEL_PROPERTY_NAMING_VALUE = "camelCase";

    @Override
    public String getLanguage() {
        return "typescript-fetch";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, SORT_PARAMS_VALUE)
                .put(CodegenConstants.ENSURE_UNIQUE_PARAMS, ENSURE_UNIQUE_PARAMS_VALUE)
                .put(CodegenConstants.MODEL_PROPERTY_NAMING, MODEL_PROPERTY_NAMING_VALUE)
                .put(CodegenConstants.SUPPORTS_ES6, String.valueOf(SUPPORTS_ES6_VALUE))
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
