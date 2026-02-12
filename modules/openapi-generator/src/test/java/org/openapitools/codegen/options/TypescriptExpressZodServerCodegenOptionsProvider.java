package org.openapitools.codegen.options;

import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.languages.TypescriptExpressZodServerCodegen;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class TypescriptExpressZodServerCodegenOptionsProvider implements OptionsProvider {
    public static final String PROJECT_NAME_VALUE = "OpenAPI";

    @Override
    public String getLanguage() {
        return "typescript-express-zod-server";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.build();
    }

    @Override
    public boolean isServer() {
        return true;
    }
}
