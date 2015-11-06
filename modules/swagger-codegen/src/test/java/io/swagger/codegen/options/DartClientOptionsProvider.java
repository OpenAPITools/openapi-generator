package io.swagger.codegen.options;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.DartClientCodegen;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class DartClientOptionsProvider implements OptionsProvider {
    public static final String SORT_PARAMS_VALUE = "false";
    public static final String ENSURE_UNIQUE_PARAMS_VALUE = "true";
    public static final String BROWSER_CLIENT_VALUE = "true";
    public static final String PUB_NAME_VALUE = "swagger";
    public static final String PUB_VERSION_VALUE = "1.0.0-SNAPSHOT";
    public static final String PUB_DESCRIPTION_VALUE = "Swagger API client dart";
    public static final String SOURCE_FOLDER_VALUE = "src";

    @Override
    public String getLanguage() {
        return "dart";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, SORT_PARAMS_VALUE)
                .put(CodegenConstants.ENSURE_UNIQUE_PARAMS, ENSURE_UNIQUE_PARAMS_VALUE)
                .put(DartClientCodegen.BROWSER_CLIENT, BROWSER_CLIENT_VALUE)
                .put(DartClientCodegen.PUB_NAME, PUB_NAME_VALUE)
                .put(DartClientCodegen.PUB_VERSION, PUB_VERSION_VALUE)
                .put(DartClientCodegen.PUB_DESCRIPTION, PUB_DESCRIPTION_VALUE)
                .put(CodegenConstants.SOURCE_FOLDER, SOURCE_FOLDER_VALUE)
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
