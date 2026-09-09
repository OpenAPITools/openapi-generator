package org.openapitools.codegen.options;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class TypeScriptExpressZodServerCodegenOptionsProvider implements TypeScriptSharedClientOptionsProvider {
    public static final String PROJECT_NAME_VALUE = "OpenAPI";

    @Override
    public String getLanguage() {
        return "typescript-express-zod-server";
    }

    @Override
    public Map<String, String> createOptions() {
        return ImmutableMap.<String, String>builder()
                .putAll(TypeScriptSharedClientOptionsProvider.super.createOptions())
                .build();
    }

    @Override
    public boolean isServer() {
        return true;
    }
}
