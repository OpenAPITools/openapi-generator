package org.openapitools.codegen.options;

import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.languages.DartNextClientCodegen;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class DartNextClientCodegenOptionsProvider implements OptionsProvider {
    public static final String PROJECT_NAME_VALUE = "OpenAPI";

    @Override
    public String getLanguage() {
        return "dart-next";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder
                .put(DartNextClientCodegen.PROJECT_NAME, PROJECT_NAME_VALUE)
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}

