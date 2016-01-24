package io.swagger.codegen.options;

import io.swagger.codegen.CodegenConstants;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class CSharpClientOptionsProvider implements OptionsProvider {
    public static final String PACKAGE_NAME_VALUE = "swagger_client_csharp";
    public static final String PACKAGE_VERSION_VALUE = "1.0.0-SNAPSHOT";
    public static final String SOURCE_FOLDER_VALUE = "src_csharp";
    public static final String PACKAGE_GUID_VALUE = "{894EAEBB-649A-498C-A735-10D0BD7B73E0}";
	
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
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
