package org.openapitools.codegen.options;

import java.util.Map;

import org.openapitools.codegen.languages.N4jsClientCodegen;

import com.google.common.collect.ImmutableMap;

public class N4jsClientCodegenOptionsProvider implements OptionsProvider {
    public static final String PROJECT_NAME_VALUE = "OpenAPI";
    public static final String CHECK_REQUIRED_BODY_PROPS_NOT_NULL__VALUE = "false";
    public static final String CHECK_SUPERFLUOUS_BODY_PROPS__VALUE = "false";

    @Override
    public String getLanguage() {
        return "n4js";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder
                .put(N4jsClientCodegen.PROJECT_NAME, PROJECT_NAME_VALUE)
                .put(N4jsClientCodegen.CHECK_REQUIRED_BODY_PROPS_NOT_NULL, CHECK_REQUIRED_BODY_PROPS_NOT_NULL__VALUE)
                .put(N4jsClientCodegen.CHECK_SUPERFLUOUS_BODY_PROPS, CHECK_SUPERFLUOUS_BODY_PROPS__VALUE)
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}

