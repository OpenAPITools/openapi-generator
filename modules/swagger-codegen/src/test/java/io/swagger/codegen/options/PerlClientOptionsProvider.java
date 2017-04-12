package io.swagger.codegen.options;

import io.swagger.codegen.CodegenConstants;

import io.swagger.codegen.languages.PerlClientCodegen;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class PerlClientOptionsProvider implements OptionsProvider {
    public static final String MODULE_NAME_VALUE = "";
    public static final String MODULE_VERSION_VALUE = "";

    @Override
    public String getLanguage() {
        return "perl";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(PerlClientCodegen.MODULE_NAME, MODULE_NAME_VALUE)
                .put(PerlClientCodegen.MODULE_VERSION, MODULE_VERSION_VALUE)
                .put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, "true")
                .put(CodegenConstants.ENSURE_UNIQUE_PARAMS, "true")
                .put("hideGenerationTimestamp", "true")
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
