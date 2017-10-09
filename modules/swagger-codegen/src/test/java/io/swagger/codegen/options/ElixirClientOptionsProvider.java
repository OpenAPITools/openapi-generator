package io.swagger.codegen.options;

import com.google.common.collect.ImmutableMap;
import io.swagger.codegen.CodegenConstants;

import java.util.Map;

public class ElixirClientOptionsProvider implements OptionsProvider {
    public static final String INVOKER_PACKAGE_VALUE = "Yay.Pets";

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
                .put(CodegenConstants.INVOKER_PACKAGE, "Yay.Pets")
                .put("licenseHeader", "# Copyright 2017 Me\n#\n# Licensed under the Apache License")
                .put(CodegenConstants.PACKAGE_NAME, "yay_pets")
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
