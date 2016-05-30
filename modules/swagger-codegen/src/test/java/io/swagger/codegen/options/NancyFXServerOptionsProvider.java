package io.swagger.codegen.options;

import static io.swagger.codegen.CodegenConstants.PACKAGE_NAME;
import static io.swagger.codegen.CodegenConstants.PACKAGE_VERSION;
import static io.swagger.codegen.CodegenConstants.RETURN_ICOLLECTION;
import static io.swagger.codegen.CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG;
import static io.swagger.codegen.CodegenConstants.SOURCE_FOLDER;
import static io.swagger.codegen.CodegenConstants.USE_COLLECTION;
import static io.swagger.codegen.CodegenConstants.USE_DATETIME_OFFSET;

import java.util.Map;

import com.google.common.collect.ImmutableMap;

public class NancyFXServerOptionsProvider implements OptionsProvider {
    public static final String PACKAGE_NAME_VALUE = "swagger_server_nancyfx";
    public static final String PACKAGE_VERSION_VALUE = "1.0.0-SNAPSHOT";
    public static final String SOURCE_FOLDER_VALUE = "src_nancyfx";

    @Override
    public String getLanguage() {
        return "nancyfx";
    }

    @Override
    public Map<String, String> createOptions() {
        final ImmutableMap.Builder<String, String> builder = ImmutableMap.builder();
        return builder.put(PACKAGE_NAME, PACKAGE_NAME_VALUE)
                .put(PACKAGE_VERSION, PACKAGE_VERSION_VALUE)
                .put(SOURCE_FOLDER, SOURCE_FOLDER_VALUE)
                .put(SORT_PARAMS_BY_REQUIRED_FLAG, "true")
                .put(USE_DATETIME_OFFSET, "true")
                .put(USE_COLLECTION, "false")
                .put(RETURN_ICOLLECTION, "false")
                .build();
    }

    @Override
    public boolean isServer() {
        return true;
    }
}
