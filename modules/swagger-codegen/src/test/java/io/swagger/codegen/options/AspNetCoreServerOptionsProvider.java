package io.swagger.codegen.options;

import com.google.common.collect.ImmutableMap;
import io.swagger.codegen.CodegenConstants;

import java.util.Map;

public class AspNetCoreServerOptionsProvider implements OptionsProvider {
    public static final String PROJECT_GUID_VALUE = "{0FBE6C2F-40D5-4F36-85B0-365EBF0D7EE3}";
    public static final String PACKAGE_NAME_VALUE = "swagger_server_aspnetcore";
    public static final String PACKAGE_VERSION_VALUE = "1.0.0-SNAPSHOT";
    public static final String SOURCE_FOLDER_VALUE = "src_aspnetcore";

    @Override
    public String getLanguage() {
        return "aspnetcore";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(CodegenConstants.PACKAGE_NAME, PACKAGE_NAME_VALUE)
                .put(CodegenConstants.PACKAGE_VERSION, PACKAGE_VERSION_VALUE)
                .put(CodegenConstants.SOURCE_FOLDER, SOURCE_FOLDER_VALUE)
                .put(CodegenConstants.OPTIONAL_PROJECT_GUID, PROJECT_GUID_VALUE)
                .put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, "true")
                .put(CodegenConstants.USE_DATETIME_OFFSET, "true")
                .put(CodegenConstants.USE_COLLECTION, "false")
                .put(CodegenConstants.RETURN_ICOLLECTION, "false")
                .build();
    }

    @Override
    public boolean isServer() {
        return true;
    }
}
