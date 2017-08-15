package io.swagger.codegen.options;

import io.swagger.codegen.CodegenConstants;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class CSharpClientOptionsProvider implements OptionsProvider {
    public static final String PACKAGE_NAME_VALUE = "swagger_client_csharp";
    public static final String PACKAGE_VERSION_VALUE = "1.0.0-SNAPSHOT";
    public static final String SOURCE_FOLDER_VALUE = "src_csharp";
    public static final String PACKAGE_GUID_VALUE = "{894EAEBB-649A-498C-A735-10D0BD7B73E0}";
    public static final String ALLOW_UNICODE_IDENTIFIERS_VALUE = "false";
    public static final String MODEL_PROPERTY_NAMING = "modelPropertyNaming";

    @Override
    public String getLanguage() {
        return "csharp";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(CodegenConstants.PACKAGE_NAME, PACKAGE_NAME_VALUE)
                .put(CodegenConstants.PACKAGE_VERSION, PACKAGE_VERSION_VALUE)
                .put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, "true")
                .put(CodegenConstants.OPTIONAL_METHOD_ARGUMENT, "true")
                .put(CodegenConstants.OPTIONAL_ASSEMBLY_INFO, "true")
                .put(CodegenConstants.USE_DATETIME_OFFSET, "true")
                .put(CodegenConstants.SOURCE_FOLDER, SOURCE_FOLDER_VALUE)
                .put(CodegenConstants.USE_COLLECTION, "false")
                .put(CodegenConstants.RETURN_ICOLLECTION, "false")
                .put(CodegenConstants.OPTIONAL_PROJECT_FILE, "true")
                .put(CodegenConstants.OPTIONAL_PROJECT_GUID, PACKAGE_GUID_VALUE)
                .put(CodegenConstants.DOTNET_FRAMEWORK, "4.x")
                .put(CodegenConstants.OPTIONAL_EMIT_DEFAULT_VALUES, "true")
                .put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, "true")
                .put(CodegenConstants.GENERATE_PROPERTY_CHANGED, "true")
                .put(CodegenConstants.NON_PUBLIC_API, "true")
                .put(CodegenConstants.INTERFACE_PREFIX, "X")
                .put(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS, ALLOW_UNICODE_IDENTIFIERS_VALUE)
                .put(CodegenConstants.NETCORE_PROJECT_FILE, "false")
                .put(CodegenConstants.MODEL_PROPERTY_NAMING, MODEL_PROPERTY_NAMING)
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
