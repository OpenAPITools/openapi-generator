package io.swagger.codegen.options;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.PythonClientCodegen;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class PythonClientOptionsProvider implements OptionsProvider {
    public static final String PACKAGE_NAME_VALUE = "swagger_client_python";
    public static final String PROJECT_NAME_VALUE = "swagger-client-python";
    public static final String PACKAGE_VERSION_VALUE = "1.0.0-SNAPSHOT";
    public static final String PACKAGE_URL_VALUE = "";

    @Override
    public String getLanguage() {
        return "python";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(PythonClientCodegen.PACKAGE_URL, PACKAGE_URL_VALUE)
                .put(CodegenConstants.PACKAGE_NAME, PACKAGE_NAME_VALUE)
                .put(CodegenConstants.PROJECT_NAME, PROJECT_NAME_VALUE)
                .put(CodegenConstants.PACKAGE_VERSION, PACKAGE_VERSION_VALUE)
                .put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, "true")
                .put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, "true")
                .put(CodegenConstants.LIBRARY, "urllib3")
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
