package org.openapitools.codegen.options;

import com.google.common.collect.ImmutableMap;
import org.openapitools.codegen.CodegenConstants;

import java.util.Map;

public class ElixirClientOptionsProvider implements OptionsProvider {
    public static final String INVOKER_PACKAGE_VALUE = "Yay.Pets";
    public static final String PREPEND_FORM_OR_BODY_PARAMETERS_VALUE = "true";

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
                .put(CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS, PREPEND_FORM_OR_BODY_PARAMETERS_VALUE)
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
