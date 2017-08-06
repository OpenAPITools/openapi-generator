package io.swagger.codegen.options;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.RustClientCodegen;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class RustClientOptionsProvider implements OptionsProvider {
    public static final String PACKAGE_NAME_VALUE = "swagger_test";
    public static final String PACKAGE_VERSION_VALUE = "2.1.2";


    @Override
    public String getLanguage() {
        return "rust";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(RustClientCodegen.PACKAGE_NAME, PACKAGE_NAME_VALUE)
                .put(RustClientCodegen.PACKAGE_VERSION, PACKAGE_VERSION_VALUE)
                .put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, "true")
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
