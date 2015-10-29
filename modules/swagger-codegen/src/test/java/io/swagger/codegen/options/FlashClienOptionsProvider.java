package io.swagger.codegen.options;

import io.swagger.codegen.CodegenConstants;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class FlashClienOptionsProvider implements OptionsProvider {
    public static final String PACKAGE_NAME_VALUE = "io.swagger.flash";
    public static final String PACKAGE_VERSION_VALUE = "1.0.0-SNAPSHOT";
    public static final String INVOKER_PACKAGE_VALUE = "io.swagger.flash";
    public static final String SOURCE_FOLDER_VALUE = "src/main/flex/test";

    @Override
    public String getLanguage() {
        return "flash";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(CodegenConstants.PACKAGE_NAME, PACKAGE_NAME_VALUE)
                .put(CodegenConstants.PACKAGE_VERSION, PACKAGE_VERSION_VALUE)
                .put(CodegenConstants.INVOKER_PACKAGE, INVOKER_PACKAGE_VALUE)
                .put(CodegenConstants.SOURCE_FOLDER, SOURCE_FOLDER_VALUE)
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
