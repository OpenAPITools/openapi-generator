package io.swagger.codegen.options;

import java.util.Map;

import com.google.common.collect.ImmutableMap;

import io.swagger.codegen.CodegenConstants;

public class EiffelClientOptionsProvider implements OptionsProvider {

    public static final String PACKAGE_VERSION_VALUE = "1.0.0";
    public static final String PACKAGE_NAME_VALUE = "Eiffel";

    @Override
    public String getLanguage() {
        return "eiffel";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder
                .put(CodegenConstants.PACKAGE_VERSION, PACKAGE_VERSION_VALUE)
                .put(CodegenConstants.PACKAGE_NAME, PACKAGE_NAME_VALUE)
                .put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, "true")
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }

}
