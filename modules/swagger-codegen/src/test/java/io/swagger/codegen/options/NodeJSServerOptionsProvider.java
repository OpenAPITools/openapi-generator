package io.swagger.codegen.options;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.NodeJSServerCodegen;
import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class NodeJSServerOptionsProvider implements OptionsProvider {
    public static final String SORT_PARAMS_VALUE = "false";
    public static final String ENSURE_UNIQUE_PARAMS_VALUE = "true";
    public static final String GOOGLE_CLOUD_FUNCTIONS = "false";
    public static final String EXPORTED_NAME = "exported";
    public static final String ALLOW_UNICODE_IDENTIFIERS_VALUE = "false";
    public static final String SERVER_PORT = "8080";


    @Override
    public String getLanguage() {
        return "nodejs-server";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, SORT_PARAMS_VALUE)
                .put(CodegenConstants.ENSURE_UNIQUE_PARAMS, ENSURE_UNIQUE_PARAMS_VALUE)
                .put(NodeJSServerCodegen.GOOGLE_CLOUD_FUNCTIONS, GOOGLE_CLOUD_FUNCTIONS)
                .put(NodeJSServerCodegen.EXPORTED_NAME, EXPORTED_NAME)
                .put(NodeJSServerCodegen.SERVER_PORT, SERVER_PORT)
                .put(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS, ALLOW_UNICODE_IDENTIFIERS_VALUE)
                .build();
    }

    @Override
    public boolean isServer() {
        return true;
    }
}
