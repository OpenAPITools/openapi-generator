package io.swagger.codegen.options;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.GoClientCodegen;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class GoClientOptionsProvider implements OptionsProvider {

    public static final String PACKAGE_VERSION_VALUE = "1.0.0";
    public static final String PACKAGE_NAME_VALUE = "Go";

    @Override
    public String getLanguage() {
        return "go";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder
                .put(CodegenConstants.PACKAGE_VERSION, PACKAGE_VERSION_VALUE)
                .put(CodegenConstants.PACKAGE_NAME, PACKAGE_NAME_VALUE)
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
