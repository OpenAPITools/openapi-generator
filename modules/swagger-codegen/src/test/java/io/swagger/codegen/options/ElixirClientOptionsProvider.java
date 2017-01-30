package io.swagger.codegen.options;

import com.google.common.collect.ImmutableMap;
import io.swagger.codegen.CodegenConstants;

import java.util.Map;

public class ElixirClientOptionsProvider implements OptionsProvider {

    @Override
    public String getLanguage() {
        return "elixir";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder
                .put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, "false")
                .put(CodegenConstants.ENSURE_UNIQUE_PARAMS, "false")
                .put(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS, "false")
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
